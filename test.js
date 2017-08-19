#! env node
//           `--::-.`
//       ./shddddddddhs+.
//     :yddddddddddddddddy:
//   `sdddddddddddddddddddds`
//  /ddddy:oddddddddds:sddddd/   @By: Debray Arnaud <adebray> - adebray@student.42.fr
//  sdddddddddddddddddddddddds   @Last modified by: adebray
//  sdddddddddddddddddddddddds
//  :ddddddddddhyyddddddddddd:   @Created: 2017-08-17T23:47:18+02:00
//   odddddddd/`:-`sdddddddds    @Modified: 2017-08-19T03:09:11+02:00
//    +ddddddh`+dh +dddddddo
//     -sdddddh///sdddddds-
//       .+ydddddddddhs/.
//           .-::::-`

// Utils

const log = o => console.log(require('util').inspect(o, { colors: true, depth: null }))

//

if (process.argv.length != 4) {
	console.log('usage: ./test.js jsonfile input')
	process.exit(-1)
}

let out = ""
let refs = {}

refs.__proto__._ = []
for (var i = 48; i < 58; i++)
	refs.__proto__._.push( String.fromCharCode(i) )
for (var i = 65; i < 91; i++)
	refs.__proto__._.push( String.fromCharCode(i) )
for (var i = 97; i < 122; i++)
	refs.__proto__._.push( String.fromCharCode(i) )

refs.__proto__.push = function (s) {
	let e = _.shift()
	while (Object.keys(this).some(_ => _ == e))
		e = _.shift()
	this[e] = s
}
refs.__proto__.indexOf = function (s) {
	return Object.keys(this).reduce( (p, k) => {
		if (this[k] == s)
			return k
		else return p
	}, null)
}
let data = require('./' + process.argv[2])

Object.keys(data).forEach( e => {
	out += out[out.length - 1] == '|' ? "" : "|"
	let v = data[e]
	if ( e == 'name') {}
	else if (v instanceof Array) {
		if (e == 'states') {
			out += v.reduce( (p, e) => {
				refs.push(e)
				return p += refs.indexOf(e)
			}, "")
		}
		else if (e == 'finals') {
			out += v.reduce( (p, e) => p += refs.indexOf(e), "")
		}
		else if (e == 'alphabet') {
			out += v.reduce( (p, e) => {
				refs[e] = true
				return p += e
			}, "")
		}
		else {
			out += v.reduce( (p, e) => p += e, "")
		}
	}
	else if (typeof v == 'string')
		if (e == 'initial')
			out += refs.indexOf(v)
		else
			out += v
	else if (v instanceof Object) {
		Object.keys(v).forEach( e => v[e].forEach( _ =>
			out += refs.indexOf(e) + _.read + refs.indexOf(_.to_state) + _.write + _.action[0]
		) )
	}
})
out += '|' + process.argv[3] + '|~~'
console.log(out)
console.log("-----------")

let alphabet = () => Object.keys( Array.from(out).reduce( (p,e) => {
	if (!p[e])
		p[e] = true
	return p
}, {} ) )

let small_alphabet = () => Object.keys( Array.from(data.alphabet).reduce( (p,e) => {
	if (!p[e])
		p[e] = true
	return p
}, {} ) )

let anything_but = function (chars, next, action) {
	let res = []
	alphabet().forEach( e => {
		if (!(new RegExp(`[${chars}]`).test(e)))
			res.push({read: e, to_state: next, write: e, action})
	})
	return res
}

let check_alpha = function (chars, next, action) {
	for (var i = 0; i < Object.keys(data).length; i++) {
		if (i + 1 < Object.keys(data).length)
			this[`CHECK_ALPHA_${chars}_${i}`] = anything_but('|', `CHECK_ALPHA_${chars}_${i}`, 'LEFT').concat([
					{read:'|', to_state: `CHECK_ALPHA_${chars}_${i + 1}`, write: '|', action: 'LEFT'}
			])
		else
			this[`CHECK_ALPHA_${chars}_${i}`] = anything_but(chars + '|', `CHECK_ALPHA_${chars}_${i}`, 'LEFT').concat([
					{read:chars, to_state: `MOVEEND`, write: chars, action: 'RIGHT'},
					{read:'|', to_state: `HALT`, write: '|', action: 'LEFT'}
			])
	}
	return anything_but('', `CHECK_ALPHA_${chars}_${0}`, 'LEFT')
}

let transitions = {
	'MOVEEND' : function () {
		return anything_but("~", "MOVEEND", "RIGHT").concat([
			{read: '~', to_state: 'JUMPLEFT', write: '~', action: 'LEFT'}
		])
	},
	'JUMPLEFT' : function () {
		return anything_but("", "CHECK_ALPHA", "LEFT")
	},
	'CHECK_ALPHA': function () {
		let res = []
		small_alphabet().forEach( e => {
			res.push({read: e, to_state: `CHECK_ALPHA_${e}`, write: e, action: 'LEFT'})
			this[`CHECK_ALPHA_${e}`] = check_alpha.call(this, e)
		})
		return res
	}
}

transitions = Object.keys(transitions).reduce( (p, k) => {
	let v = transitions[k]
	p[k] = v.call(p)
	return p
}, {})

console.log(transitions)

let dt = {
	'name': 'sim_' + process.argv[2].match(/\/([^/]*)$/)[1],
	'alphabet': alphabet(),
	'blank': '~',
	'states': Object.keys(transitions).concat(['HALT']),
	'initial': 'MOVEEND',
	'finals': [ 'HALT' ],
	'transitions': transitions
}

log( dt )

require('fs').writeFileSync('input_' + process.argv[2].match(/\/([^/]*)$/)[1], out)
require('fs').writeFileSync('sim_' + process.argv[2].match(/\/([^/]*)$/)[1], JSON.stringify(dt, null, "  "))
