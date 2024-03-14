const { MichelsonMap } = require("@taquito/taquito")

const AccessController = artifacts.require("AccessController")
const PausableToken = artifacts.require("PausableToken")

module.exports = async deployer => {
    const controller = await AccessController.deployed()
    console.log('Address: ', controller.address)
    await deployer.deploy(
        PausableToken,
        {
            ledger: MichelsonMap.fromLiteral({
                "mv19hGkQPN277aqUikmFUz4fXSzYKa2P58Fd": 100
            }),
            owner: controller.address,
            paused: false
        }
    )
}
