{ dockerTools, writeShellScriptBin, runCommand, bash, ligo
, name ? "ligo", extraContents ? [ ] }:
dockerTools.buildImage {
  inherit name;
  tag = "latest";

  fromImageName = "scratch";

  contents = [ ligo bash ] ++ extraContents;
  config.Entrypoint = name;
}
