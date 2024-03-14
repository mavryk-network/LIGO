const AccessController = artifacts.require("AccessController")

module.exports = async deployer => {
    await deployer.deploy(AccessController, [
        "mv19hGkQPN277aqUikmFUz4fXSzYKa2P58Fd"
    ])
}
