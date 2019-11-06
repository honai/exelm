require('./index.html')

const Elm = require('./Main.elm').Elm
const app = Elm.Main.init({
  node: document.getElementById('main')
})
