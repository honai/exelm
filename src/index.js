require('./index.html')

const Elm = require('./Main.elm').Elm

const storageKey = '0.1'

const app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: JSON.parse(localStorage.getItem(storageKey))
})
window.addEventListener("keydown", function(e) {
  const scrollKeys = ['ArrowUp', 'ArrowDown', ' ']
  if (scrollKeys.includes(e.key)) {
    e.preventDefault()
  }
})
app.ports.saveTable.subscribe(function(data) {
  console.log(data)
  localStorage.setItem(storageKey, JSON.stringify(data))
})
