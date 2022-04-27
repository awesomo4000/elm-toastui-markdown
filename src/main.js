import "./main.css";
import "./ToastUiEditor";
import { Elm } from "./Main.elm";

const app = Elm.Main.init({ flags: {} });
app.ports.log.subscribe(message => console.log(message))
console.log("[main.js] load.")
