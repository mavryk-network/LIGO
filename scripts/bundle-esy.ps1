param($Path)
$NpmPrefix = $(npm config get prefix)
$EsyLocation = $NpmPrefix + '\node_modules\esy'
$BundledEsyLocation = $Path + '\node_modules\esy';
$EsyCmdWrapper = $NpmPrefix + "\esy.ps1"
$EsyPs1Wrapper = $NpmPrefix + "\esy.ps1"
echo "Clearing any previously bundled esy"
rm -Recurse -Force -ErrorAction SilentlyContinue $BundledEsyLocation
cp -Recurse $EsyLocation $BundledEsyLocation
echo "Copying $EsyPs1Wrapper and $EsyCmdwrapper to $BundledEsyLocation"
cp $EsyPs1Wrapper, $EsyCmdWrapper $Path
