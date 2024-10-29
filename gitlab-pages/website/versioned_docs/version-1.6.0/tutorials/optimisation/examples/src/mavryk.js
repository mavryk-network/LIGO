const fs = require("fs")
const path = require("path")
const { promisify } = require("util")
const readFile = promisify(fs.readFile)

const taquito = require("@mavrykdynamics/taquito")
const { InMemorySigner } = require("@mavrykdynamics/taquito-signer")

async function initializeMavryk(networkConfig) {
    const uri = `${networkConfig.host}:${networkConfig.port}`
    const Mavryk = new taquito.MavrykToolkit(uri)
    Mavryk.setProvider({
        signer: new InMemorySigner(networkConfig.secretKey),
    })
    return Mavryk
}

class Artifacts {
    constructor(Mavryk, buildDir) {
        this.Mavryk = Mavryk
        this.buildDir = buildDir
        this.chainId = null
    }

    async getContract(name) {
        const artifactFile = path.join(
            this.buildDir, "contracts", `${name}.json`
        )
        const artifact = JSON.parse(await readFile(artifactFile))
        if (this.chainId === null) {
            this.chainId = await this.Mavryk.rpc.getChainId()
        }
        let address = null
        try {
            address = artifact.networks[this.chainId].address
        } catch (err) {
            console.error(
                `Could not find the address of ${name}`
            )
            throw err
        }
        return this.Mavryk.contract.at(address)
    }
}

module.exports = { initializeMavryk, Artifacts }