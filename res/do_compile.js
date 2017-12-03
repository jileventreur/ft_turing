//           `--::-.`
//       ./shddddddddhs+.
//     :yddddddddddddddddy:
//   `sdddddddddddddddddddds`
//  /ddddy:oddddddddds:sddddd/   @By: Debray Arnaud <adebray> - adebray@student.42.fr
//  sdddddddddddddddddddddddds   @Last modified by: adebray
//  sdddddddddddddddddddddddds
//  :ddddddddddhyyddddddddddd:   @Created: 2017-08-27T16:50:49+02:00
//   odddddddd/`:-`sdddddddds    @Modified: 2017-08-27T17:48:39+02:00
//    +ddddddh`+dh +dddddddo
//     -sdddddh///sdddddds-
//       .+ydddddddddhs/.
//           .-::::-`

if (process.argv.length != 4)
	console.log("usage: ./do_compile checkjson execjson")

let check = require("../" + process.argv[3])
let exec = require("../" + process.argv[2])

check.transitions["ERASE"] = check.transitions["ERASE"].map( e => {
	if (e.to_state == "TEST")
		e.to_state = exec.initial
	return e
})
check.states = check.states.concat(exec.states).filter(function(item, pos, a) {
	return a.indexOf(item) == pos;
})
check.finals = check.finals.concat(exec.finals).filter(function(item, pos, a) {
	return a.indexOf(item) == pos;
})
check.transitions = Object.assign(check.transitions, exec.transitions)

require('fs').writeFileSync('test.json', JSON.stringify(check, null, "  "))
