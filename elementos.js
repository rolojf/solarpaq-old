#!/usr/bin/env node

const spawnCallback = require("cross-spawn").spawn;
const terser = require("terser");
const elements = 'src/CustomElements/Reto.elm';
const fs = require("fs-extra");//.promises;
const path = require("path");
const cwd = process.cwd()


process.on("unhandledRejection", (error) => {
  console.error(error);
  process.exit(1);
});


// ok rolo
async function elmToEsm(elmPath) {
  const elmEs3 = await fs.readFile(elmPath, "utf8");

  return (
    "\n" +
    "const scope = {};\n" +
    elmEs3.replace("}(this));", "}(scope));") +
    "export const { Elm } = scope;\n" +
    "\n"
  );
}


async function compileElmCustomElements() {
  const outputPath = `static/ce.js`;
  await spawnElmMake(elements, outputPath);

  const elmEsmContent = await elmToEsm(path.join(cwd, outputPath));
  const elmFileOutput = await runTerserNew(elmEsmContent);
  await fs.writeFile(path.join(process.cwd(), outputPath), elmFileOutput);
  }

// ok rolo
function spawnElmMake(elmEntrypointPath, outputPath, cwd) {
  return new Promise((resolve, reject) => {
    const fullOutputPath = cwd ? path.join(cwd, outputPath) : outputPath;
    if (fs.existsSync(fullOutputPath)) {
      fs.rmSync(fullOutputPath, {
        force: true /* ignore errors if file doesn't exist */,
      });
    }
    const subprocess = runElm(elmEntrypointPath, outputPath, cwd)

    subprocess.on("close", (code) => {
      const fileOutputExists = fs.existsSync(fullOutputPath);
      if (code == 0 && fileOutputExists) {
        resolve();
      } else {
        reject();
        process.exit(1);
      }
    });
  });
}


// ok rolo
function runElm(elmEntrypointPath, outputPath, cwd) {
    return spawnCallback(
      `elm-optimize-level-2`,
      [elmEntrypointPath, "--output", outputPath],
      {
        // ignore stdout
        stdio: ["inherit", "ignore", "inherit"],
        cwd: cwd,
      }
    );
  }


// ok rolo
async function runTerserNew(fileContents) {
  const minifiedElm = await terser.minify(fileContents, {
    ecma: 5,
    module: true,
    compress: {
      pure_funcs: [
        "F2",
        "F3",
        "F4",
        "F5",
        "F6",
        "F7",
        "F8",
        "F9",
        "A2",
        "A3",
        "A4",
        "A5",
        "A6",
        "A7",
        "A8",
        "A9",
      ],
      pure_getters: true,
      keep_fargs: false,
      unsafe_comps: true,
      unsafe: true,
    },
    mangle: true,
  });
  const code = minifiedElm.code;
  if (code) {
    return code;
  } else {
    throw "Error running terser.";
  }
}

async function run() {
   compileElmCustomElements();
}

run();

