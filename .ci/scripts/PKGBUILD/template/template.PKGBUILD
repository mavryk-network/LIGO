# Maintainer: LigoLang
pkgname=ligo-bin
pkgver=LIGO_VERSION_PLACEHOLDER
pkgrel=1
pkgdesc="High Level Smart Contract Language for Tezos (binary)"
url="https://gitlab.com/ligolang/ligo"

arch=('x86_64')
license=('MIT')

provides=('ligo')
conflicts=('ligo' 'ligo-next')

source=("https://gitlab.com/ligolang/ligo/-/jobs/LIGO_ARTIFACT_JOB_ID_PLACEHOLDER/artifacts/raw/ligo.deb")
md5sums=('SKIP')

prepare() {
        cd "$srcdir/"
        tar xvf data.tar.xz -C .
}

package() {
        cd "$srcdir/"
        cp -r usr ${pkgdir}
}
