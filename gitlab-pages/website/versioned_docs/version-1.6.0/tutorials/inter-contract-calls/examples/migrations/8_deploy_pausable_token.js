const { MichelsonMap } = require("@mavrykdynamics/webmavryk")

const AccessController = artifacts.require("AccessController")
const PausableToken = artifacts.require("PausableToken")

module.exports = async deployer => {
    const controller = await AccessController.deployed()
    console.log('Address: ', controller.address)
    await deployer.deploy(
        PausableToken,
        {
            ledger: MichelsonMap.fromLiteral({
                "mv1D1eVV688difqMJENSEnon47yBpJM7JS7N": 100
            }),
            owner: controller.address,
            paused: false
        }
    )
}
