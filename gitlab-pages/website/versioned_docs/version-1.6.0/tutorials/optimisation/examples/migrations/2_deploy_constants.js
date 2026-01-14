const webmavryk = require('@mavrykdynamics/webmavryk')

const ConstantsV1 = artifacts.require("ConstantsV1")
const ConstantsV2 = artifacts.require("ConstantsV2")

module.exports = async (deployer)  => {
    await deployer.deploy(ConstantsV1, webmavryk.UnitValue)
    await deployer.deploy(ConstantsV2, webmavryk.UnitValue)
}