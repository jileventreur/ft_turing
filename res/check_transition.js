//           `--::-.`
//       ./shddddddddhs+.
//     :yddddddddddddddddy:
//   `sdddddddddddddddddddds`
//  /ddddy:oddddddddds:sddddd/   @By: Debray Arnaud <adebray> - adebray@student.42.fr
//  sdddddddddddddddddddddddds   @Last modified by: adebray
//  sdddddddddddddddddddddddds
//  :ddddddddddhyyddddddddddd:   @Created: 2017-08-27T16:39:28+02:00
//   odddddddd/`:-`sdddddddds    @Modified: 2017-08-27T16:44:46+02:00
//    +ddddddh`+dh +dddddddo
//     -sdddddh///sdddddds-
//       .+ydddddddddhs/.
//           .-::::-`

let check = require('./test.json')

let keys = Object.keys(check.transitions)
keys.forEach( k => {
	console.log( check.transitions[k].to_state )
})
// console.log(check.transitions)
