const bert = require('node-bertrpc')

bert.connect(10001, 'localhost', function (erl) {

  // Get a hold of the modules
  const example = erl.mod('example')
  const dictoid = erl.mod('dictoid')

  example.call('adder', [1,2]).finish( function (result) {
    console.log(' > example:adder(1,2) ')
    console.log(' < '+result)
  })

  dictoid.call('to_dict', ["aberth", "is", "great!"]).finish( function (result) {
    console.log(' > dictoid:to_dict(["aberth", "is", "great!"]) ')
    console.log(' < '+JSON.stringify(result))
  })

})
