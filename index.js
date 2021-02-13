import "./beta-style.css";

// @ts-ignore
const { Elm } = require("./src/Main.elm");
const pagesInit = require("../elm-pages/index.js");

pagesInit({
  mainElmModule: Elm.Main
});

