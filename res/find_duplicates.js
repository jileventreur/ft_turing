//           `--::-.`
//       ./shddddddddhs+.
//     :yddddddddddddddddy:
//   `sdddddddddddddddddddds`
//  /ddddy:oddddddddds:sddddd/   @By: Debray Arnaud <adebray> - adebray@student.42.fr
//  sdddddddddddddddddddddddds   @Last modified by: adebray
//  sdddddddddddddddddddddddds
//  :ddddddddddhyyddddddddddd:   @Created: 2017-08-26T16:57:08+02:00
//   odddddddd/`:-`sdddddddds    @Modified: 2017-08-27T16:43:07+02:00
//    +ddddddh`+dh +dddddddo
//     -sdddddh///sdddddds-
//       .+ydddddddddhs/.
//           .-::::-`

let transitions = require('./test.json').transitions

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
