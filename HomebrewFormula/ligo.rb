class Ligo < Formula
  desc "Friendly Smart Contract Language for Tezos"
  homepage "https://ligolang.org/"
  license "MIT"

  # We clone repo explicitely to preserve the information about git submodules
  url "https://gitlab.com/ligolang/ligo.git", tag: "0.71.0", revision: "bdd69fa7cd9adc8756ca9e688ede0335672e29cf"
  version "0.71.0"
  head "https://gitlab.com/ligolang/ligo.git", branch: "dev"


  bottle do
    root_url "https://gitlab.com/api/v4/projects/12294987/packages/generic/ligo_bottle/current"
  sha256 cellar: :any, arm64_ventura: "c56bb5dd203515a4c0861d5a4f2d34ce216dc3f4c4fc37d4294be5ec0680cc3b"
  sha256 cellar: :any, ventura: "d432824ac3d974ad7b7564ce80f71aed20dc9b263fa8967c7f5500ab5dbe0f17"
  end

  build_dependencies = %w[opam rust hidapi pkg-config gnu-sed cmake gcc]
  build_dependencies.each do |dependency|
    depends_on dependency => :build
  end

  dependencies = %w[gmp libev libffi]
  dependencies.each do |dependency|
    depends_on dependency
  end

  # sets up env vars for opam before running a command
  private def with_opam_env(cmd)
    "eval \"$(opam config env)\" && #{cmd}"
  end

  def install
    # ligo version is taken from the environment variable in build-time
    ENV["LIGO_VERSION"] = "0.71.0"
    # avoid opam prompts
    ENV["OPAMYES"] = "true"

    # init opam state in ~/.opam
    system "opam", "init", "--bare", "--auto-setup", "--disable-sandboxing"
    # create opam switch with required ocaml version
    system "scripts/setup_switch.sh"
    # TODO: remowe workarounds below and use the script provided by the ligo repo once
    # a new version is released
    # Required for Tezos hangzhou protocol
    system "git", "submodule", "init"
    system "git", "submodule", "update", "--recursive"
    # Because sed has different options on MacOS >:(
    system "gsed -i 's/sed/gsed/g' scripts/install_vendors_deps.sh"
    # Build dependencies
    system with_opam_env "scripts/install_vendors_deps.sh"
    # build ligo
    system with_opam_env "dune build -p ligo"

    # install ligo binary
    cp "_build/install/default/bin/ligo", "ligo"
    bin.mkpath
    bin.install "ligo"
  end

  test do
    system "#{bin}/ligo", "--help"
  end
end
