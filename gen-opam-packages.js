let esyJson = require("./esy-backup.json");

let keys = Object.keys(esyJson.resolutions)
  .filter(
    (k) =>
      (typeof esyJson.resolutions[k] === "string" &&
        esyJson.resolutions[k] === "./vendors/tezos-ligo") ||
      (typeof esyJson.resolutions[k] === "object" &&
        esyJson.resolutions[k].source === "./vendors/tezos-ligo")
  )
  .map((k) => k.replace("@opam/", ""));

console.log(keys.join("\n"));
