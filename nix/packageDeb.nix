# Create a debian package from static executable
{ stdenv, lib, writeTextFile, dpkg, ligo }:
let
  project = "ligo";
  version = ligo.version;
  # revision = lib.sources.commitIdFromGitRepo ../.git;
  pkgArch = "amd64";
  bin = "${ligo}/bin/ligo";
  pkgName = "${project}_0ubuntu${version}_${pkgArch}";
  depends = "";
  maintainer = "ligolang ligolang.org";
  description = "A friendly Smart Contract Language for Tezos";

  writeControlFile = writeTextFile {
    name = "control";
    text = ''
      Package: ${project}
      Version: ${version}
      Priority: optional
      Architecture: ${pkgArch}
      Depends: ${depends}
      Maintainer: ${maintainer}
      Description: ${project}
       ${description}
    '';
  };

in stdenv.mkDerivation rec {
  name = "${pkgName}.deb";

  nativeBuildInputs = [ dpkg ];

  phases = "packagePhase";

  packagePhase = ''
    mkdir ${pkgName}
    mkdir -p ${pkgName}/usr/local/bin
    cp ${bin} ${pkgName}/usr/local/bin/${project}

    mkdir ${pkgName}/DEBIAN
    cp ${writeControlFile} ${pkgName}/DEBIAN/control

    dpkg-deb --build ${pkgName}
    mkdir -p $out
    cp ${name} $out/
    ln $out/${name} $out/${project}.deb
  '';
}
