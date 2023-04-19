###### Description of changes \n\nUpdate ligo version to latest one \n\n###### Things done \n\n- Built on platform(s)\n  - [ ] x86_64-linux\n  - [ ] aarch64-linux\n  - [ ] x86_64-darwin\n  - [ ] aarch64-darwin\n- [ ] For non-Linux: Is `sandbox = true` set in `nix.conf`? (See [Nix manual](https://nixos.org/manual/nix/stable/command-ref/conf-file.html))\n- [ ] Tested, as applicable:\n  - [NixOS test(s)](https://nixos.org/manual/nixos/unstable/index.html#sec-nixos-tests) (look inside [nixos/tests](https://github.com/NixOS/nixpkgs/blob/master/nixos/tests))\n  - and/or [package tests](https://nixos.org/manual/nixpkgs/unstable/#sec-package-tests)\n  - or, for functions and \"core\" functionality, tests in [lib/tests](https://github.com/NixOS/nixpkgs/blob/master/lib/tests) or [pkgs/test](https://github.com/NixOS/nixpkgs/blob/master/pkgs/test)\n  - made sure NixOS tests are [linked](https://nixos.org/manual/nixpkgs/unstable/#ssec-nixos-tests-linking) to the relevant packages\n- [ ] Tested compilation of all packages that depend on this change using `nix-shell -p nixpkgs-review --run \"nixpkgs-review rev HEAD\"`. Note: all changes have to be committed, also see [nixpkgs-review usage](https://github.com/Mic92/nixpkgs-review#usage)\n- [ ] Tested basic functionality of all binary files (usually in `./result/bin/`)\n- [23.05 Release Notes (or backporting 22.11 Release notes)](https://github.com/NixOS/nixpkgs/blob/master/CONTRIBUTING.md#generating-2305-release-notes)\n  - [ ] (Package updates) Added a release notes entry if the change is major or breaking\n  - [ ] (Module updates) Added a release notes entry if the change is significant\n  - [ ] (Module addition) Added a release notes entry if adding a new NixOS module\n- [x] Fits [CONTRIBUTING.md](https://github.com/NixOS/nixpkgs/blob/master/CONTRIBUTING.md).\n
