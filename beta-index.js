import { Elm as Elm2 } from "./ce.js"
export default function (elmLoaded) {
  console.log("Hello outside of promise!");
  class ElReto extends HTMLElement {
    constructor() {
      super();
    };
    connectedCallback() {
      // definimos este como el customElement
      const este = this;
      este.innerHTML = '';
      este.appendChild(document.createElement('div'));
      const app2 = Elm2.CustomElements.Reto.init({node: este.children[0]});
      app2.ports.responde.subscribe(function(valor){
        const evento = new CustomEvent('responde',
                                       { detail: { resultado : valor }});
        este.dispatchEvent(evento);
      });
    }
  };
  window.customElements.define("el-reto", ElReto);
  elmLoaded.then((elmPagesApp) => {
    console.log("Inside of promise");
    //      elmPagesApp.ports.example.subscribe((message) => {
    console.log("Elm port message:...");
    //      });
  });
}
