/* tslint:disable */
/* eslint-disable */
/**
 * WebIde backend API
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 1.0.0
 * Contact: hi@serokell.io
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 */


import type { Configuration } from './configuration';
import type { AxiosPromise, AxiosInstance, AxiosRequestConfig } from 'axios';
import globalAxios from 'axios';
// Some imports not used depending on template conditions
// @ts-ignore
import { DUMMY_BASE_URL, assertParamExists, setApiKeyToObject, setBasicAuthToObject, setBearerAuthToObject, setOAuthToObject, setSearchParams, serializeDataIfNeeded, toPathString, createRequestFunction } from './common';
import type { RequestArgs } from './base';
// @ts-ignore
import { BASE_PATH, COLLECTION_FORMATS, BaseAPI, RequiredError } from './base';

/**
 * 
 * @export
 * @interface CompileExpressionRequest
 */
export interface CompileExpressionRequest {
    /**
     * 
     * @type {DisplayFormat}
     * @memberof CompileExpressionRequest
     */
    'displayFormat'?: DisplayFormat;
    /**
     * 
     * @type {string}
     * @memberof CompileExpressionRequest
     */
    'function': string;
    /**
     * 
     * @type {Project}
     * @memberof CompileExpressionRequest
     */
    'project': Project;
    /**
     * 
     * @type {string}
     * @memberof CompileExpressionRequest
     */
    'protocol'?: string;
}


/**
 * 
 * @export
 * @interface CompileRequest
 */
export interface CompileRequest {
    /**
     * 
     * @type {DisplayFormat}
     * @memberof CompileRequest
     */
    'displayFormat'?: DisplayFormat;
    /**
     * 
     * @type {string}
     * @memberof CompileRequest
     */
    'entrypoint'?: string;
    /**
     * 
     * @type {Project}
     * @memberof CompileRequest
     */
    'project': Project;
    /**
     * 
     * @type {string}
     * @memberof CompileRequest
     */
    'protocol'?: string;
    /**
     * 
     * @type {string}
     * @memberof CompileRequest
     */
    'storage'?: string;
}


/**
 * 
 * @export
 * @interface DeployScript
 */
export interface DeployScript {
    /**
     * 
     * @type {string}
     * @memberof DeployScript
     */
    'build': string;
    /**
     * 
     * @type {string}
     * @memberof DeployScript
     */
    'script': string;
}
/**
 * 
 * @export
 * @enum {string}
 */

export const DisplayFormat = {
    Dev: 'dev',
    Json: 'json',
    HumanReadable: 'humanReadable'
} as const;

export type DisplayFormat = typeof DisplayFormat[keyof typeof DisplayFormat];


/**
 * 
 * @export
 * @interface DryRunRequest
 */
export interface DryRunRequest {
    /**
     * 
     * @type {DisplayFormat}
     * @memberof DryRunRequest
     */
    'displayFormat'?: DisplayFormat;
    /**
     * 
     * @type {string}
     * @memberof DryRunRequest
     */
    'parameters': string;
    /**
     * 
     * @type {Project}
     * @memberof DryRunRequest
     */
    'project': Project;
    /**
     * 
     * @type {string}
     * @memberof DryRunRequest
     */
    'protocol'?: string;
    /**
     * 
     * @type {string}
     * @memberof DryRunRequest
     */
    'storage': string;
}


/**
 * 
 * @export
 * @interface GenerateDeployScriptRequest
 */
export interface GenerateDeployScriptRequest {
    /**
     * 
     * @type {string}
     * @memberof GenerateDeployScriptRequest
     */
    'entrypoint'?: string;
    /**
     * 
     * @type {string}
     * @memberof GenerateDeployScriptRequest
     */
    'name': string;
    /**
     * 
     * @type {Project}
     * @memberof GenerateDeployScriptRequest
     */
    'project': Project;
    /**
     * 
     * @type {string}
     * @memberof GenerateDeployScriptRequest
     */
    'protocol'?: string;
    /**
     * 
     * @type {string}
     * @memberof GenerateDeployScriptRequest
     */
    'storage': string;
}
/**
 * 
 * @export
 * @interface GistCreateUpdateRequest
 */
export interface GistCreateUpdateRequest {
    /**
     * 
     * @type {string}
     * @memberof GistCreateUpdateRequest
     */
    'description'?: string;
    /**
     * 
     * @type {string}
     * @memberof GistCreateUpdateRequest
     */
    'gistId'?: string;
    /**
     * 
     * @type {Array<SourceFile>}
     * @memberof GistCreateUpdateRequest
     */
    'sourceFiles': Array<SourceFile>;
}
/**
 * 
 * @export
 * @interface ListDeclarationsRequest
 */
export interface ListDeclarationsRequest {
    /**
     * 
     * @type {boolean}
     * @memberof ListDeclarationsRequest
     */
    'onlyEndpoint'?: boolean;
    /**
     * 
     * @type {Project}
     * @memberof ListDeclarationsRequest
     */
    'project': Project;
}
/**
 * 
 * @export
 * @interface Project
 */
export interface Project {
    /**
     * 
     * @type {string}
     * @memberof Project
     */
    'main': string;
    /**
     * 
     * @type {string}
     * @memberof Project
     */
    'module'?: string;
    /**
     * 
     * @type {Array<SourceFile>}
     * @memberof Project
     */
    'sourceFiles': Array<SourceFile>;
}
/**
 * 
 * @export
 * @interface RunTestRequest
 */
export interface RunTestRequest {
    /**
     * 
     * @type {DisplayFormat}
     * @memberof RunTestRequest
     */
    'displayFormat'?: DisplayFormat;
    /**
     * 
     * @type {Project}
     * @memberof RunTestRequest
     */
    'project': Project;
    /**
     * 
     * @type {string}
     * @memberof RunTestRequest
     */
    'testFilePath': string;
}


/**
 * 
 * @export
 * @interface SourceFile
 */
export interface SourceFile {
    /**
     * 
     * @type {string}
     * @memberof SourceFile
     */
    'filePath': string;
    /**
     * 
     * @type {string}
     * @memberof SourceFile
     */
    'source': string;
}

/**
 * DefaultApi - axios parameter creator
 * @export
 */
export const DefaultApiAxiosParamCreator = function (configuration?: Configuration) {
    return {
        /**
         * 
         * @param {CompileExpressionRequest} [compileExpressionRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        compileExpressionPost: async (compileExpressionRequest?: CompileExpressionRequest, options: AxiosRequestConfig = {}): Promise<RequestArgs> => {
            const localVarPath = `/compile-expression`;
            // use dummy base URL string because the URL constructor only accepts absolute URLs.
            const localVarUrlObj = new URL(localVarPath, DUMMY_BASE_URL);
            let baseOptions;
            if (configuration) {
                baseOptions = configuration.baseOptions;
            }

            const localVarRequestOptions = { method: 'POST', ...baseOptions, ...options};
            const localVarHeaderParameter = {} as any;
            const localVarQueryParameter = {} as any;


    
            localVarHeaderParameter['Content-Type'] = 'application/json;charset=utf-8';

            setSearchParams(localVarUrlObj, localVarQueryParameter);
            let headersFromBaseOptions = baseOptions && baseOptions.headers ? baseOptions.headers : {};
            localVarRequestOptions.headers = {...localVarHeaderParameter, ...headersFromBaseOptions, ...options.headers};
            localVarRequestOptions.data = serializeDataIfNeeded(compileExpressionRequest, localVarRequestOptions, configuration)

            return {
                url: toPathString(localVarUrlObj),
                options: localVarRequestOptions,
            };
        },
        /**
         * 
         * @param {CompileRequest} [compileRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        compilePost: async (compileRequest?: CompileRequest, options: AxiosRequestConfig = {}): Promise<RequestArgs> => {
            const localVarPath = `/compile`;
            // use dummy base URL string because the URL constructor only accepts absolute URLs.
            const localVarUrlObj = new URL(localVarPath, DUMMY_BASE_URL);
            let baseOptions;
            if (configuration) {
                baseOptions = configuration.baseOptions;
            }

            const localVarRequestOptions = { method: 'POST', ...baseOptions, ...options};
            const localVarHeaderParameter = {} as any;
            const localVarQueryParameter = {} as any;


    
            localVarHeaderParameter['Content-Type'] = 'application/json;charset=utf-8';

            setSearchParams(localVarUrlObj, localVarQueryParameter);
            let headersFromBaseOptions = baseOptions && baseOptions.headers ? baseOptions.headers : {};
            localVarRequestOptions.headers = {...localVarHeaderParameter, ...headersFromBaseOptions, ...options.headers};
            localVarRequestOptions.data = serializeDataIfNeeded(compileRequest, localVarRequestOptions, configuration)

            return {
                url: toPathString(localVarUrlObj),
                options: localVarRequestOptions,
            };
        },
        /**
         * 
         * @param {GistCreateUpdateRequest} [gistCreateUpdateRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        createUpdateGistPost: async (gistCreateUpdateRequest?: GistCreateUpdateRequest, options: AxiosRequestConfig = {}): Promise<RequestArgs> => {
            const localVarPath = `/create-update-gist`;
            // use dummy base URL string because the URL constructor only accepts absolute URLs.
            const localVarUrlObj = new URL(localVarPath, DUMMY_BASE_URL);
            let baseOptions;
            if (configuration) {
                baseOptions = configuration.baseOptions;
            }

            const localVarRequestOptions = { method: 'POST', ...baseOptions, ...options};
            const localVarHeaderParameter = {} as any;
            const localVarQueryParameter = {} as any;


    
            localVarHeaderParameter['Content-Type'] = 'application/json;charset=utf-8';

            setSearchParams(localVarUrlObj, localVarQueryParameter);
            let headersFromBaseOptions = baseOptions && baseOptions.headers ? baseOptions.headers : {};
            localVarRequestOptions.headers = {...localVarHeaderParameter, ...headersFromBaseOptions, ...options.headers};
            localVarRequestOptions.data = serializeDataIfNeeded(gistCreateUpdateRequest, localVarRequestOptions, configuration)

            return {
                url: toPathString(localVarUrlObj),
                options: localVarRequestOptions,
            };
        },
        /**
         * 
         * @param {DryRunRequest} [dryRunRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        dryRunPost: async (dryRunRequest?: DryRunRequest, options: AxiosRequestConfig = {}): Promise<RequestArgs> => {
            const localVarPath = `/dry-run`;
            // use dummy base URL string because the URL constructor only accepts absolute URLs.
            const localVarUrlObj = new URL(localVarPath, DUMMY_BASE_URL);
            let baseOptions;
            if (configuration) {
                baseOptions = configuration.baseOptions;
            }

            const localVarRequestOptions = { method: 'POST', ...baseOptions, ...options};
            const localVarHeaderParameter = {} as any;
            const localVarQueryParameter = {} as any;


    
            localVarHeaderParameter['Content-Type'] = 'application/json;charset=utf-8';

            setSearchParams(localVarUrlObj, localVarQueryParameter);
            let headersFromBaseOptions = baseOptions && baseOptions.headers ? baseOptions.headers : {};
            localVarRequestOptions.headers = {...localVarHeaderParameter, ...headersFromBaseOptions, ...options.headers};
            localVarRequestOptions.data = serializeDataIfNeeded(dryRunRequest, localVarRequestOptions, configuration)

            return {
                url: toPathString(localVarUrlObj),
                options: localVarRequestOptions,
            };
        },
        /**
         * 
         * @param {GenerateDeployScriptRequest} [generateDeployScriptRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        generateDeployScriptPost: async (generateDeployScriptRequest?: GenerateDeployScriptRequest, options: AxiosRequestConfig = {}): Promise<RequestArgs> => {
            const localVarPath = `/generate-deploy-script`;
            // use dummy base URL string because the URL constructor only accepts absolute URLs.
            const localVarUrlObj = new URL(localVarPath, DUMMY_BASE_URL);
            let baseOptions;
            if (configuration) {
                baseOptions = configuration.baseOptions;
            }

            const localVarRequestOptions = { method: 'POST', ...baseOptions, ...options};
            const localVarHeaderParameter = {} as any;
            const localVarQueryParameter = {} as any;


    
            localVarHeaderParameter['Content-Type'] = 'application/json;charset=utf-8';

            setSearchParams(localVarUrlObj, localVarQueryParameter);
            let headersFromBaseOptions = baseOptions && baseOptions.headers ? baseOptions.headers : {};
            localVarRequestOptions.headers = {...localVarHeaderParameter, ...headersFromBaseOptions, ...options.headers};
            localVarRequestOptions.data = serializeDataIfNeeded(generateDeployScriptRequest, localVarRequestOptions, configuration)

            return {
                url: toPathString(localVarUrlObj),
                options: localVarRequestOptions,
            };
        },
        /**
         * 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        ligoVersionPost: async (options: AxiosRequestConfig = {}): Promise<RequestArgs> => {
            const localVarPath = `/ligo-version`;
            // use dummy base URL string because the URL constructor only accepts absolute URLs.
            const localVarUrlObj = new URL(localVarPath, DUMMY_BASE_URL);
            let baseOptions;
            if (configuration) {
                baseOptions = configuration.baseOptions;
            }

            const localVarRequestOptions = { method: 'POST', ...baseOptions, ...options};
            const localVarHeaderParameter = {} as any;
            const localVarQueryParameter = {} as any;


    
            setSearchParams(localVarUrlObj, localVarQueryParameter);
            let headersFromBaseOptions = baseOptions && baseOptions.headers ? baseOptions.headers : {};
            localVarRequestOptions.headers = {...localVarHeaderParameter, ...headersFromBaseOptions, ...options.headers};

            return {
                url: toPathString(localVarUrlObj),
                options: localVarRequestOptions,
            };
        },
        /**
         * 
         * @param {ListDeclarationsRequest} [listDeclarationsRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        listDeclarationsPost: async (listDeclarationsRequest?: ListDeclarationsRequest, options: AxiosRequestConfig = {}): Promise<RequestArgs> => {
            const localVarPath = `/list-declarations`;
            // use dummy base URL string because the URL constructor only accepts absolute URLs.
            const localVarUrlObj = new URL(localVarPath, DUMMY_BASE_URL);
            let baseOptions;
            if (configuration) {
                baseOptions = configuration.baseOptions;
            }

            const localVarRequestOptions = { method: 'POST', ...baseOptions, ...options};
            const localVarHeaderParameter = {} as any;
            const localVarQueryParameter = {} as any;


    
            localVarHeaderParameter['Content-Type'] = 'application/json;charset=utf-8';

            setSearchParams(localVarUrlObj, localVarQueryParameter);
            let headersFromBaseOptions = baseOptions && baseOptions.headers ? baseOptions.headers : {};
            localVarRequestOptions.headers = {...localVarHeaderParameter, ...headersFromBaseOptions, ...options.headers};
            localVarRequestOptions.data = serializeDataIfNeeded(listDeclarationsRequest, localVarRequestOptions, configuration)

            return {
                url: toPathString(localVarUrlObj),
                options: localVarRequestOptions,
            };
        },
        /**
         * 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        listTemplatesPost: async (options: AxiosRequestConfig = {}): Promise<RequestArgs> => {
            const localVarPath = `/list-templates`;
            // use dummy base URL string because the URL constructor only accepts absolute URLs.
            const localVarUrlObj = new URL(localVarPath, DUMMY_BASE_URL);
            let baseOptions;
            if (configuration) {
                baseOptions = configuration.baseOptions;
            }

            const localVarRequestOptions = { method: 'POST', ...baseOptions, ...options};
            const localVarHeaderParameter = {} as any;
            const localVarQueryParameter = {} as any;


    
            setSearchParams(localVarUrlObj, localVarQueryParameter);
            let headersFromBaseOptions = baseOptions && baseOptions.headers ? baseOptions.headers : {};
            localVarRequestOptions.headers = {...localVarHeaderParameter, ...headersFromBaseOptions, ...options.headers};

            return {
                url: toPathString(localVarUrlObj),
                options: localVarRequestOptions,
            };
        },
        /**
         * 
         * @param {RunTestRequest} [runTestRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        runTestPost: async (runTestRequest?: RunTestRequest, options: AxiosRequestConfig = {}): Promise<RequestArgs> => {
            const localVarPath = `/run-test`;
            // use dummy base URL string because the URL constructor only accepts absolute URLs.
            const localVarUrlObj = new URL(localVarPath, DUMMY_BASE_URL);
            let baseOptions;
            if (configuration) {
                baseOptions = configuration.baseOptions;
            }

            const localVarRequestOptions = { method: 'POST', ...baseOptions, ...options};
            const localVarHeaderParameter = {} as any;
            const localVarQueryParameter = {} as any;


    
            localVarHeaderParameter['Content-Type'] = 'application/json;charset=utf-8';

            setSearchParams(localVarUrlObj, localVarQueryParameter);
            let headersFromBaseOptions = baseOptions && baseOptions.headers ? baseOptions.headers : {};
            localVarRequestOptions.headers = {...localVarHeaderParameter, ...headersFromBaseOptions, ...options.headers};
            localVarRequestOptions.data = serializeDataIfNeeded(runTestRequest, localVarRequestOptions, configuration)

            return {
                url: toPathString(localVarUrlObj),
                options: localVarRequestOptions,
            };
        },
    }
};

/**
 * DefaultApi - functional programming interface
 * @export
 */
export const DefaultApiFp = function(configuration?: Configuration) {
    const localVarAxiosParamCreator = DefaultApiAxiosParamCreator(configuration)
    return {
        /**
         * 
         * @param {CompileExpressionRequest} [compileExpressionRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        async compileExpressionPost(compileExpressionRequest?: CompileExpressionRequest, options?: AxiosRequestConfig): Promise<(axios?: AxiosInstance, basePath?: string) => AxiosPromise<string>> {
            const localVarAxiosArgs = await localVarAxiosParamCreator.compileExpressionPost(compileExpressionRequest, options);
            return createRequestFunction(localVarAxiosArgs, globalAxios, BASE_PATH, configuration);
        },
        /**
         * 
         * @param {CompileRequest} [compileRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        async compilePost(compileRequest?: CompileRequest, options?: AxiosRequestConfig): Promise<(axios?: AxiosInstance, basePath?: string) => AxiosPromise<string>> {
            const localVarAxiosArgs = await localVarAxiosParamCreator.compilePost(compileRequest, options);
            return createRequestFunction(localVarAxiosArgs, globalAxios, BASE_PATH, configuration);
        },
        /**
         * 
         * @param {GistCreateUpdateRequest} [gistCreateUpdateRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        async createUpdateGistPost(gistCreateUpdateRequest?: GistCreateUpdateRequest, options?: AxiosRequestConfig): Promise<(axios?: AxiosInstance, basePath?: string) => AxiosPromise<string>> {
            const localVarAxiosArgs = await localVarAxiosParamCreator.createUpdateGistPost(gistCreateUpdateRequest, options);
            return createRequestFunction(localVarAxiosArgs, globalAxios, BASE_PATH, configuration);
        },
        /**
         * 
         * @param {DryRunRequest} [dryRunRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        async dryRunPost(dryRunRequest?: DryRunRequest, options?: AxiosRequestConfig): Promise<(axios?: AxiosInstance, basePath?: string) => AxiosPromise<string>> {
            const localVarAxiosArgs = await localVarAxiosParamCreator.dryRunPost(dryRunRequest, options);
            return createRequestFunction(localVarAxiosArgs, globalAxios, BASE_PATH, configuration);
        },
        /**
         * 
         * @param {GenerateDeployScriptRequest} [generateDeployScriptRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        async generateDeployScriptPost(generateDeployScriptRequest?: GenerateDeployScriptRequest, options?: AxiosRequestConfig): Promise<(axios?: AxiosInstance, basePath?: string) => AxiosPromise<DeployScript>> {
            const localVarAxiosArgs = await localVarAxiosParamCreator.generateDeployScriptPost(generateDeployScriptRequest, options);
            return createRequestFunction(localVarAxiosArgs, globalAxios, BASE_PATH, configuration);
        },
        /**
         * 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        async ligoVersionPost(options?: AxiosRequestConfig): Promise<(axios?: AxiosInstance, basePath?: string) => AxiosPromise<string>> {
            const localVarAxiosArgs = await localVarAxiosParamCreator.ligoVersionPost(options);
            return createRequestFunction(localVarAxiosArgs, globalAxios, BASE_PATH, configuration);
        },
        /**
         * 
         * @param {ListDeclarationsRequest} [listDeclarationsRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        async listDeclarationsPost(listDeclarationsRequest?: ListDeclarationsRequest, options?: AxiosRequestConfig): Promise<(axios?: AxiosInstance, basePath?: string) => AxiosPromise<Array<string>>> {
            const localVarAxiosArgs = await localVarAxiosParamCreator.listDeclarationsPost(listDeclarationsRequest, options);
            return createRequestFunction(localVarAxiosArgs, globalAxios, BASE_PATH, configuration);
        },
        /**
         * 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        async listTemplatesPost(options?: AxiosRequestConfig): Promise<(axios?: AxiosInstance, basePath?: string) => AxiosPromise<Array<string>>> {
            const localVarAxiosArgs = await localVarAxiosParamCreator.listTemplatesPost(options);
            return createRequestFunction(localVarAxiosArgs, globalAxios, BASE_PATH, configuration);
        },
        /**
         * 
         * @param {RunTestRequest} [runTestRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        async runTestPost(runTestRequest?: RunTestRequest, options?: AxiosRequestConfig): Promise<(axios?: AxiosInstance, basePath?: string) => AxiosPromise<string>> {
            const localVarAxiosArgs = await localVarAxiosParamCreator.runTestPost(runTestRequest, options);
            return createRequestFunction(localVarAxiosArgs, globalAxios, BASE_PATH, configuration);
        },
    }
};

/**
 * DefaultApi - factory interface
 * @export
 */
export const DefaultApiFactory = function (configuration?: Configuration, basePath?: string, axios?: AxiosInstance) {
    const localVarFp = DefaultApiFp(configuration)
    return {
        /**
         * 
         * @param {CompileExpressionRequest} [compileExpressionRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        compileExpressionPost(compileExpressionRequest?: CompileExpressionRequest, options?: any): AxiosPromise<string> {
            return localVarFp.compileExpressionPost(compileExpressionRequest, options).then((request) => request(axios, basePath));
        },
        /**
         * 
         * @param {CompileRequest} [compileRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        compilePost(compileRequest?: CompileRequest, options?: any): AxiosPromise<string> {
            return localVarFp.compilePost(compileRequest, options).then((request) => request(axios, basePath));
        },
        /**
         * 
         * @param {GistCreateUpdateRequest} [gistCreateUpdateRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        createUpdateGistPost(gistCreateUpdateRequest?: GistCreateUpdateRequest, options?: any): AxiosPromise<string> {
            return localVarFp.createUpdateGistPost(gistCreateUpdateRequest, options).then((request) => request(axios, basePath));
        },
        /**
         * 
         * @param {DryRunRequest} [dryRunRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        dryRunPost(dryRunRequest?: DryRunRequest, options?: any): AxiosPromise<string> {
            return localVarFp.dryRunPost(dryRunRequest, options).then((request) => request(axios, basePath));
        },
        /**
         * 
         * @param {GenerateDeployScriptRequest} [generateDeployScriptRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        generateDeployScriptPost(generateDeployScriptRequest?: GenerateDeployScriptRequest, options?: any): AxiosPromise<DeployScript> {
            return localVarFp.generateDeployScriptPost(generateDeployScriptRequest, options).then((request) => request(axios, basePath));
        },
        /**
         * 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        ligoVersionPost(options?: any): AxiosPromise<string> {
            return localVarFp.ligoVersionPost(options).then((request) => request(axios, basePath));
        },
        /**
         * 
         * @param {ListDeclarationsRequest} [listDeclarationsRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        listDeclarationsPost(listDeclarationsRequest?: ListDeclarationsRequest, options?: any): AxiosPromise<Array<string>> {
            return localVarFp.listDeclarationsPost(listDeclarationsRequest, options).then((request) => request(axios, basePath));
        },
        /**
         * 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        listTemplatesPost(options?: any): AxiosPromise<Array<string>> {
            return localVarFp.listTemplatesPost(options).then((request) => request(axios, basePath));
        },
        /**
         * 
         * @param {RunTestRequest} [runTestRequest] 
         * @param {*} [options] Override http request option.
         * @throws {RequiredError}
         */
        runTestPost(runTestRequest?: RunTestRequest, options?: any): AxiosPromise<string> {
            return localVarFp.runTestPost(runTestRequest, options).then((request) => request(axios, basePath));
        },
    };
};

/**
 * DefaultApi - object-oriented interface
 * @export
 * @class DefaultApi
 * @extends {BaseAPI}
 */
export class DefaultApi extends BaseAPI {
    /**
     * 
     * @param {CompileExpressionRequest} [compileExpressionRequest] 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof DefaultApi
     */
    public compileExpressionPost(compileExpressionRequest?: CompileExpressionRequest, options?: AxiosRequestConfig) {
        return DefaultApiFp(this.configuration).compileExpressionPost(compileExpressionRequest, options).then((request) => request(this.axios, this.basePath));
    }

    /**
     * 
     * @param {CompileRequest} [compileRequest] 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof DefaultApi
     */
    public compilePost(compileRequest?: CompileRequest, options?: AxiosRequestConfig) {
        return DefaultApiFp(this.configuration).compilePost(compileRequest, options).then((request) => request(this.axios, this.basePath));
    }

    /**
     * 
     * @param {GistCreateUpdateRequest} [gistCreateUpdateRequest] 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof DefaultApi
     */
    public createUpdateGistPost(gistCreateUpdateRequest?: GistCreateUpdateRequest, options?: AxiosRequestConfig) {
        return DefaultApiFp(this.configuration).createUpdateGistPost(gistCreateUpdateRequest, options).then((request) => request(this.axios, this.basePath));
    }

    /**
     * 
     * @param {DryRunRequest} [dryRunRequest] 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof DefaultApi
     */
    public dryRunPost(dryRunRequest?: DryRunRequest, options?: AxiosRequestConfig) {
        return DefaultApiFp(this.configuration).dryRunPost(dryRunRequest, options).then((request) => request(this.axios, this.basePath));
    }

    /**
     * 
     * @param {GenerateDeployScriptRequest} [generateDeployScriptRequest] 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof DefaultApi
     */
    public generateDeployScriptPost(generateDeployScriptRequest?: GenerateDeployScriptRequest, options?: AxiosRequestConfig) {
        return DefaultApiFp(this.configuration).generateDeployScriptPost(generateDeployScriptRequest, options).then((request) => request(this.axios, this.basePath));
    }

    /**
     * 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof DefaultApi
     */
    public ligoVersionPost(options?: AxiosRequestConfig) {
        return DefaultApiFp(this.configuration).ligoVersionPost(options).then((request) => request(this.axios, this.basePath));
    }

    /**
     * 
     * @param {ListDeclarationsRequest} [listDeclarationsRequest] 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof DefaultApi
     */
    public listDeclarationsPost(listDeclarationsRequest?: ListDeclarationsRequest, options?: AxiosRequestConfig) {
        return DefaultApiFp(this.configuration).listDeclarationsPost(listDeclarationsRequest, options).then((request) => request(this.axios, this.basePath));
    }

    /**
     * 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof DefaultApi
     */
    public listTemplatesPost(options?: AxiosRequestConfig) {
        return DefaultApiFp(this.configuration).listTemplatesPost(options).then((request) => request(this.axios, this.basePath));
    }

    /**
     * 
     * @param {RunTestRequest} [runTestRequest] 
     * @param {*} [options] Override http request option.
     * @throws {RequiredError}
     * @memberof DefaultApi
     */
    public runTestPost(runTestRequest?: RunTestRequest, options?: AxiosRequestConfig) {
        return DefaultApiFp(this.configuration).runTestPost(runTestRequest, options).then((request) => request(this.axios, this.basePath));
    }
}


