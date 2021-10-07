const createHash = require('crypto').createHash;
const glob = require('glob');
const {join, basename} = require('path')
const fs = require('fs');
const YAML = require('yamljs');

function urlFriendlyHash(content) {
  const hash = createHash('md5');
  hash.update(content);

  return hash
    .digest('base64')
    .replace(/\+|\/|=|\s|'|"/g, '-')
    .replace(/-+$/, '')
}

function convertToJson(content, path) {
  const METADATA_REGEX = /\(\*_\*(.*)\*_\*\)/ms;
  const match = content.match(METADATA_REGEX);

  if (!match || !match[1]) {
    throw new Error(`Unable to find compiler configuration in ${path}.`);
  }

  try {
    const config = YAML.parse(match[1]);
    config.editor = {
      language: config.language,
      code: content.replace(METADATA_REGEX, ''),
    };
    delete config.language;

    return config;
  } catch (ex) {
    throw new Error(`${path} doesn't contain valid metadata. ${ex}`);
  }
}

function readFile(path) {
  return new Promise((resolve, reject) => {
    fs.readFile(path, 'utf8', (error, content) => {
      if (error) {
        reject(error);
      } else {
        resolve(content);
      }
    });
  });
}

function writeFile(path, config) {
  return new Promise((resolve, reject) => {
    fs.writeFile(path, JSON.stringify(config), (error) => {
      if (error) {
        reject(error);
      } else {
        resolve();
      }
    });
  });
}

async function processExample(abspath, destDir) {

  console.log(`Processing ${abspath}`);

  const content = await readFile(abspath);
  const config = convertToJson(content, basename(abspath));
  const id = urlFriendlyHash(basename(abspath));
  console.log(id)

  config.id = id;

  await writeFile(join(destDir, id), config);

  return { id: id, name: config.name };
}

function globPromise(src) {
  return new Promise((resolve, reject) => {
    glob(src, (err, matches) => {
      err
        ? reject(err)
        : resolve(matches)
    })  
  })
}

async function processExamples(srcDir, exclusions, destDir) {
  const src = join(srcDir, "/**/*.*ligo")
  const retval = (await globPromise(src))
    .sort((a, b) => basename(a).localeCompare(basename(b)))
    .map(item => {
      console.log(item);
      return item;
    })
    .reduce(
      (retval, abspath) => {
        const relpath = basename(abspath)
        return exclusions.includes(relpath)
            ? retval
            : [...retval, processExample(abspath, destDir)]  
      },
      []
    )
  return Promise.all(retval)
}

async function main() {
  process.on('unhandledRejection', (error) => {
    throw error;
  });

  const EXAMPLES_DIR =
    process.env['EXAMPLES_DIR'] ||
    join(process.cwd(), '../../../../src/test/examples');

  const EXAMPLES_DEST_DIR = join(process.cwd(), 'build', 'static', 'examples');
  fs.mkdirSync(EXAMPLES_DEST_DIR, { recursive: true });

  const EXCLUSIONS=[
    // "0-increment.jsligo",
    // "id.jsligo"
  ]

  const examples = await processExamples(
    EXAMPLES_DIR,
    EXCLUSIONS,
    EXAMPLES_DEST_DIR
  );

  const EXAMPLES_LIST_FILE = 'list';
  await writeFile(join(EXAMPLES_DEST_DIR, EXAMPLES_LIST_FILE), examples);
}

main();
