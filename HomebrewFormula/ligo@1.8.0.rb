class LigoAT180 < Formula
  desc "Friendly Smart Contract Language for Mavryk"
  homepage "https://ligo.mavryk.org/"
  license "MIT"

  # We clone repo explicitely to preserve the information about git submodules
  # MAVRYK: 1.8.0 (PascaLIGO restoration + union types). Update `revision` to the exact commit tagged
  # 1.8.0 (e.g. the dev merge commit); the value below is pascaligo-restoration HEAD at prep time.
  url "https://gitlab.com/mavryk-network/ligo.git", tag: "1.8.0", revision: "81fdd5b229f916b9b0a4be2396034ab0f16af33f"
  version "1.8.0"
  head "https://gitlab.com/mavryk-network/ligo.git", branch: "dev"

  # MAVRYK: 1.8.0 bottles hosted on the mavryk-network/ligo registry (project 51776731). Only
  # arm64_tahoe published so far; other platforms build from source until their bottle is added.
  bottle do
    root_url "https://gitlab.com/api/v4/projects/51776731/packages/generic/ligo_bottle/current"
    sha256 cellar: :any, arm64_tahoe: "26bfae1944fd039542fd17eb27a190e706704f34bc6b77d700df84c9da87e2ea"
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
    ENV["LIGO_VERSION"] = "1.8.0"
    # avoid opam prompts
    ENV["OPAMYES"] = "true"

    # init opam state in ~/.opam
    system "opam", "init", "--bare", "--auto-setup", "--disable-sandboxing"
    # create opam switch with required ocaml version
    system "scripts/setup_switch.sh"
    # TODO: remowe workarounds below and use the script provided by the ligo repo once
    # a new version is released
    # Required for Mavryk hangzhou protocol
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
