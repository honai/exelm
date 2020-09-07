import { Elm } from './Main.elm'

const storageKey = '0.1'

const app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: JSON.parse(localStorage.getItem(storageKey))
})
app.ports.saveTable.subscribe(function(data) {
  console.log(data)
  localStorage.setItem(storageKey, JSON.stringify(data))
})
