#if INTERACTIVE
#r "nuget: NuGet.Protocol"
#r "nuget: NuGet.Resolver"
#else
module ServerNuGet
#endif

open System.Collections.Concurrent
open System.Threading
open NuGet.Common
open NuGet.Frameworks
open NuGet.Packaging.Core
open NuGet.Protocol
open NuGet.Protocol.Core.Types
open NuGet.Resolver
open NuGet.Versioning

type Package = {
    Id : string
    Version : string
}

type PackageTree = {
    Package : Package
    Dependencies : PackageTree list
}

let resolvePackages (package : PackageIdentity) (framework : NuGetFramework) (repository : SourceRepository) (logger : ILogger) =
    async {
        use cacheContext = new SourceCacheContext()
        cacheContext.IgnoreFailedSources <- true
        cacheContext.NoCache <- true
        cacheContext.DirectDownload <- true

        let! dependencyInfoResource = repository.GetResourceAsync<DependencyInfoResource>() |> Async.AwaitTask
        let resolvedPackages = ConcurrentDictionary<PackageIdentity, SourcePackageDependencyInfo>(PackageIdentityComparer.Default)

        let rec resolvePackage (package : PackageIdentity) =
            async {
                let! dependencyInfo =
                    dependencyInfoResource.ResolvePackage(package, framework, cacheContext, logger, CancellationToken.None)
                    |> Async.AwaitTask

                if (dependencyInfo <> null) && (resolvedPackages.TryAdd(PackageIdentity(dependencyInfo.Id, dependencyInfo.Version), dependencyInfo)) then
                    return! dependencyInfo.Dependencies
                            |> Seq.map (fun dependency ->
                                resolvePackage (PackageIdentity(dependency.Id, dependency.VersionRange.MinVersion)))
                            |> Async.Parallel
                            |> Async.Ignore
            }

        // Resolve packages
        let! _ = resolvePackage package

        return resolvedPackages
    }

let prunePackages (package : PackageIdentity) (packages : seq<SourcePackageDependencyInfo>) (repository : SourceRepository) (logger : ILogger) =
    let resolverContext =
            PackageResolverContext(
                DependencyBehavior.Lowest,
                seq { package.Id },
                Seq.empty,
                Seq.empty,
                Seq.empty,
                packages,
                seq { repository.PackageSource },
                logger)

    let resolver = PackageResolver()

    resolver.Resolve(resolverContext, CancellationToken.None)
    |> Seq.map(fun x -> (x.Id, x))
    |> Map.ofSeq

let getGraph (p : Package) (f : string) =
    async {
        let repository = Repository.Factory.GetCoreV3("https://api.nuget.org/v3/index.json")
        let logger = NullLogger.Instance
        let framework = NuGetFramework.Parse(f)
        let package = PackageIdentity(p.Id, NuGetVersion(p.Version))

        let! resolvedPackages = resolvePackages package framework repository logger
        let prunedPackages = prunePackages package (resolvedPackages.Values |> Seq.distinct) repository logger

        let rec createTree (p : PackageIdentity) =
            let package = prunedPackages.[p.Id]
            let dependencies =
                resolvedPackages.[package].Dependencies
                |> Seq.map (fun d -> prunedPackages.[d.Id])
                |> Seq.map createTree
                |> Seq.toList

            {
                Package = { Id = package.Id; Version = package.Version.ToNormalizedString() }
                Dependencies = dependencies
            }

        return createTree package
    }