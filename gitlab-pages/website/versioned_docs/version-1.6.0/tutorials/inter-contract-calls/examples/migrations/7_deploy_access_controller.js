const AccessController = artifacts.require("AccessController")

module.exports = async deployer => {
    await deployer.deploy(AccessController, [
        "mv1D1eVV688difqMJENSEnon47yBpJM7JS7N"
    ])
}
