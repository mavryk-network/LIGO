{ lib }:

rec {
  filterSource = { src, dirs ? [ ], files ? [ ] }: (lib.cleanSourceWith {
    inherit src;
    # Good examples: https://github.com/NixOS/nixpkgs/blob/master/lib/sources.nix
    filter = name: type:
      let
        path = toString name;
        baseName = baseNameOf path;
        relPath = lib.removePrefix (toString src + "/") path;
      in
      lib.any (dir: dir == relPath || (lib.hasPrefix "${dir}/" relPath)) dirs ||
      (type == "regular" && (lib.any (file: file == baseName) files));
  });

  filterGitSource = args: lib.cleanSourceWith {
    filter = lib.cleanSourceFilter;
    src = filterSource args;
  };
}