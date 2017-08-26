let transitions = require('./sim_ispair.json').transitions

Object.keys(transitions).forEach( k => {
	let _ = {}
	transitions[k].forEach( v => {
		if (!_[v.read])
			_[v.read] = true
		else {
			console.log(k)
		}
	})

} )
