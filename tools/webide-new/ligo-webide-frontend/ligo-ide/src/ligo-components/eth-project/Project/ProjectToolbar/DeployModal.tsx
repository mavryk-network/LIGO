import React, { useState } from "react";
import { Modal } from "~/base-components/ui-components";
import { KeypairInputSelector } from "~/base-components/keypair";
import { networkManager } from "~/ligo-components/eth-network";
import fileOps from "~/base-components/file-ops";
import notification from "~/base-components/notification";
import { WebIdeApi } from "~/components/api/api";
import { ActionParamFormGroup } from "~/ligo-components/eth-contract";

interface DeployModalProps {
  modalRef: React.RefObject<Modal>;
  projectSettings: any;
  signer: string;
  projectManager: any;
}

const DeployModal: React.FC<DeployModalProps> = ({
  modalRef,
  projectSettings,
  signer,
  projectManager,
}: DeployModalProps): React.ReactElement | null => {
  const [storage, setStorage] = useState<string>("");
  const [loading, setLoading] = useState<boolean>(false);
  const [result, setResult] = useState<string>("");
  const [isWallet, setIsWallet] = useState<boolean>(false);
  const [selectedSigner, setSelectedSigner] = useState<string>("");
  const [delegateAddress, setDelegateAddress] = useState<string>("");
  const [txOptions, setTxOptions] = useState<{ [name: string]: string } | undefined>(
    // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    networkManager.sdk?.utils.txOptions?.list.reduce(
      (a: { [name: string]: string }, opt: { name: string }) => ({ ...a, [opt.name]: "" }),
      {}
    )
  );
  const [needEstimate, setNeedEstimate] = useState<boolean>(true);
  const [balance, setBalance] = useState<string>("");
  const [loadedCompiledStorage, setLoadedCompiledStorage] = useState<string>("");
  const [compiledContract, setCompiledContract] = useState<string>("");
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
  const tzFilePath: string = projectSettings?.get("deploy") || "";
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
  const mainFilePath: string = projectSettings?.get("main") || "";

  const refreshStorage = async () => {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    const storageFilePath: string = projectSettings?.get("storage") || "";
    if (storageFilePath) {
      await fileOps
        .readFile(
          // eslint-disable-next-line @typescript-eslint/restrict-template-expressions, @typescript-eslint/no-unsafe-member-access
          `${projectManager.projectRoot}/${storageFilePath.substring(2, storageFilePath.length)}`
        )
        .then((storageFileContent) => {
          setStorage(storageFileContent);
        })
        .catch((e) => {
          if (e instanceof Error) {
            notification.error("Deployment error", e.message);
          } else {
            console.error(e);
          }
        });
    }
  };

  const deploy = async () => {
    setLoading(true);

    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call, @typescript-eslint/no-unsafe-assignment
    const contractAddress: string = await networkManager.sdk.deployContract({
      type: "origination",
      tzfile: compiledContract,
      storage: loadedCompiledStorage,
      selectedSigner,
      isWallet,
      delegateAddress,
      balance,
      gasLimit: txOptions?.gasLimit,
      storageLimit: txOptions?.storageLimit,
      suggestedFeeMutez: txOptions?.fee,
    });

    setResult(contractAddress);

    setLoading(false);
  };

  const estimate = async () => {
    setNeedEstimate(true);
    setLoading(true);

    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    const contractFiles: any[] = await projectManager.getMainContract();

    if (tzFilePath && storage && networkManager.sdk) {
      const compiledStorage = await WebIdeApi.compileContract({
        project: {
          // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
          sourceFiles: contractFiles,
          // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access
          main: projectManager.mainFilePath,
        },
        storage,
      })
        .then((resp) => {
          return resp.data;
        })
        .catch((e: Error) => {
          modalRef.current?.closeModal().catch((me: Error) => {
            console.error(me);
          });
          notification.error("Deploy Error", e.message);
        });

      if (compiledStorage) {
        setLoadedCompiledStorage(compiledStorage);
      } else {
        return false;
      }

      const tzfile = await fileOps.readFile(
        // eslint-disable-next-line @typescript-eslint/restrict-template-expressions, @typescript-eslint/no-unsafe-member-access
        `${projectManager.projectRoot}/${tzFilePath.substring(2, tzFilePath.length)}`
      );

      setCompiledContract(tzfile);

      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
      const estimation = await networkManager.sdk.estimate({
        type: "origination",
        selectedSigner,
        isWallet,
        tzfile,
        storage: compiledStorage,
      });

      /* eslint-disable */
      setTxOptions({
        gasLimit: `${estimation.gasLimit}`,
        storageLimit: `${estimation.storageLimit}`,
        fee: `${estimation.suggestedFeeMutez}`,
      });
      /* eslint-enable */
    }
    setLoading(false);
    return true;
  };

  const confirmDeployment = async () => {
    if (needEstimate) {
      const estimated: boolean = await estimate().catch((e: any) => {
        if (e instanceof Error) {
          notification.error("Estimation error", e.message);
        } else {
          console.error(e);
        }
        setLoading(false);
        return false;
      });
      if (estimated) {
        setNeedEstimate(false);
      }
      return;
    }

    await deploy().catch((e: any) => {
      if (e instanceof Error) {
        notification.error("Deployment error", e.message);
      } else {
        console.error(e);
      }
      setLoading(false);
    });
  };

  return (
    <Modal
      ref={modalRef}
      title="Deploy contract"
      textConfirm={needEstimate ? "Estimate" : "Deploy"}
      pending={loading && (needEstimate ? "Estimating" : "Deploying")}
      confirmDisabled={storage === "" || selectedSigner === ""}
      onConfirm={confirmDeployment}
      onCancel={() => {
        setLoading(false);
        setStorage("");
        setResult("");
        setIsWallet(false);
        setSelectedSigner("");
        setDelegateAddress("");
        setTxOptions(
          // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
          networkManager.sdk?.utils.txOptions?.list.reduce(
            (a: { [name: string]: string }, opt: { name: string }) => ({ ...a, [opt.name]: "" }),
            {}
          )
        );
        setNeedEstimate(true);
        setBalance("");
        setLoadedCompiledStorage("");
        setCompiledContract("");
        return true;
      }}
      onOpened={refreshStorage}
      textActions={!needEstimate && storage !== "" ? ["Re-estimate"] : []}
      onActions={!needEstimate && storage !== "" ? [estimate] : []}
    >
      <div>
        During this action <kbd>{mainFilePath}</kbd> contract compiled to <kbd>{tzFilePath}</kbd>{" "}
        will be deployed.
      </div>
      <br />
      <ActionParamFormGroup
        key="deploy-param-storage"
        className="mb-2"
        label={<div>Init storage</div>}
        value={storage}
        onChange={(st: string) => setStorage(st)}
        placeholder="Storage"
        size=""
        type="string"
      />
      <KeypairInputSelector
        label="Signer"
        extra={
          networkManager.isWallet &&
          signer && [
            {
              group: networkManager.browserExtension.name.toLowerCase(),
              badge: networkManager.browserExtension.name,
              children: [
                {
                  address: signer,
                  name: networkManager.browserExtension.name,
                  onClick: () => {
                    setIsWallet(true);
                    setSelectedSigner(signer);
                  },
                },
              ],
            },
          ]
        }
        value={selectedSigner}
        onChange={(newSigner: string) => {
          setIsWallet(false);
          setSelectedSigner(newSigner);
        }}
      />
      <ActionParamFormGroup
        key="deploy-param-balance"
        className="mb-2"
        label="Balance (optional)"
        value={balance}
        onChange={(b: string) => setBalance(b)}
        placeholder="Balance"
        size=""
        type="int"
      />
      <ActionParamFormGroup
        key="deploy-param-address"
        className="mb-2"
        label={<div>Delegate address (optional)</div>}
        value={delegateAddress}
        onChange={(addr: string) => setDelegateAddress(addr)}
        placeholder="Address"
        icon="fas fa-map-marker-alt"
        size=""
        type=""
      />
      <div className="row">
        {/* eslint-disable */}
        {networkManager.sdk?.utils.txOptions?.list.map((option: any) => (
          <ActionParamFormGroup
            key={`deploy-param-${option.name}`}
            className={option.className}
            label={option.label}
            icon={option.icon}
            value={txOptions ? txOptions[option.name] : ""}
            onChange={(v: string) => setTxOptions({...txOptions, [option.name]: v })}
            placeholder={option.placeholder}
            size=""
            type=""
          />
        ))}
        {/* eslint-enable */}
      </div>
      {result && (
        <p>
          Contract <kbd>{result}</kbd> was deployed. If the network is supported, you can find it on{" "}
          <a href={`//better-call.dev/search?text=${result}`} target="_blank" rel="noreferrer">
            Better Call Dev
          </a>
        </p>
      )}
    </Modal>
  );
};

export default DeployModal;
