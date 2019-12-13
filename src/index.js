require('./index.html')

const Elm = require('./Main.elm').Elm
const app = Elm.Main.init({
  node: document.getElementById('main'),
  flags: ""
})
window.addEventListener("keydown", function(e) {
  const scrollKeys = ['ArrowUp', 'ArrowDown', ' ']
  if (scrollKeys.includes(e.key)) {
    e.preventDefault()
  }
})
