$hash = git show-ref -s v3.4.0
if (-not $?) { throw "git show-ref failed"}
$count = git rev-list --count "$hash..HEAD"
if (-not $?) { throw "git rev-list failed"}
$prefix = "3.3.$(([int]$count) + 1000)"
echo $prefix

$outdir = "./ssp-out"
if (test-path $outdir) { del $outdir -rec -for }
dotnet pack --output $outdir --configuration Release "/p:VersionPrefix=$prefix" /p:VersionSuffix=storage-schema-preview ./neo-devpack-dotnet.sln
if (-not $?) { throw "dotnet pack failed"}
