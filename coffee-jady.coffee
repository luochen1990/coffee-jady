log = do ->
	logs = []
	window.log = log if window? # window env used, for debugging easier in broswer console.
	foo = (args...) ->
		op = if args.slice(-1)[0] in ['log', 'warn', 'error'] then args.pop() else 'log'
		ball = []
		for f in args
			if typeof f == 'function'
				expr = f.toString().replace(/^\s*function\s?\(\s?\)\s?{\s*return\s*([^]*?);?\s*}$/, '$1')
				expr = expr.replace(/[\r\n]{1,2}\s*/g, '') if expr.length <= 100
				ball.push("## #{expr} ==>", f())
			else
				ball.push('##', f)
		console[op] ball...
		logs.push(ball)
	foo.logs = logs
	foo

bool = (x) -> if x == true or x == false then x else null

dict = (pairs) -> #constract object from list of pairs; recover the lack of dict comprehensions
	d = {}
	d[k] = v for [k, v] in pairs
	d

extend = (base, defaults) ->
	r = if base? then dict([k, v] for k, v of base) else {}
	r[k] ?= v for k, v of defaults if defaults? # null value will be replaced if a default value exists.
	r

accumulate = (fruit, nutri, foo) ->
	fruit = foo(fruit, it) for it in nutri
	fruit

String::cut = (start_pat, end_pat) ->
    i = @.search(start_pat) + 1
    return null if i == 0
    j = @.substr(i).search(end_pat)
    return null if j == -1
    @.substr(i, j)

close_self = new Object

jady = do ->
	(opts = {}) ->
		indent_str = opts.indent_str ? '    '

		join_inline = opts.join_inline ? do ->
			ls = ['a', 'abbr', 'acronym', 'b', 'br', 'code', 'em', 'font', 'i', 'img', 'ins', 'kbd', 'map', 'samp', 'small', 'span', 'strong', 'sub', 'sup']
			is_inline_tag = new RegExp("^(#{ls.join('|')})$")
			(tag) ->
				is_inline_tag.test(tag)

		self_closing = opts.self_closing ? do ->
			ls = ['br', 'input']
			is_self_closing_tag = new RegExp("^(#{ls.join('|')})$")
			(tag) ->
				is_self_closing_tag.test(tag)

		type = (x) ->
			t_x = typeof x
			if not x?
				return 'null'
			else if t_x isnt 'object'
				return t_x # 'string' or 'number'
			else if x instanceof Array
				return 'array'
			else if x instanceof Function
				return 'function'
			else if x is close_self
				return 'close'
			'object'

		log -> type(undefined)
		log -> type(null)
		log -> type(close_self)
		log -> type('abc')

		is_content = (arg) ->
			type(arg) != 'object'

		curry = (callback) ->
			got = []
			rec = (arg) ->
				got.push(arg)
				if is_content(arg)
					return callback(got...)
				else
					return rec
			rec

		#f = curry (args...) ->
		#	log -> args
		#	args
		#log -> f(1)(2)(3)

		expand_content = do ->
			rec_get = (got) ->
				rec = (content) ->
					t_content = type(content)
					if t_content not in ['array', 'object', 'function']
						got.push(content) if t_content not in ['null', 'close']
					else if t_content == 'function'
						#log -> content
						#log -> content()
						rec(content())
					else if t_content == 'array'
						for it in content
							rec(it)
			(content) ->
				got = []
				rec_get(got)(content)
				r = ''
				for s, i in got
					r += '\n' if i and not join_inline(s.cut(/^</, /[ >]/))
					r += s
				return r

		tag2str = do ->
			snake = (s) ->
				s.replace(/[A-Z]/g, (c) -> "-#{c}").toLowerCase()

			(label, attr, content) ->
				id = null; classes = []
				tag = (label.replace /[#.]([a-zA-Z0-9_-]+)/g, (s, m) ->
					if s[0] == '#'
						id = m
					else if s[0] == '.'
						classes.push(m)
					''
				) or 'div'
				#log -> [id, classes]
				if type(attr.class) == 'object'
					classes = classes.concat(((if v then k else null) for k, v of attr.class).filter((x) -> x?))
				else if type(attr.class) == 'string'
					classes = classes.concat(attr.class.split(' '))
				#log -> classes
				attr = extend (id: (id ? attr.id), class: classes.join(' ') or null), attr
				attrstr = ((if bool(v) then " #{snake(k)}" else " #{snake(k)}=\"#{v}\"") for k, v of attr when v? and (not bool(v)? or bool(v))).join('')

				if (content is close_self) or (not content? and self_closing(tag))
					"<#{tag}#{attrstr}>"
				else
					content = expand_content(content)
					if content.length > 60 or content.search(/\n/) != -1
						content = "\n#{(s.replace(/^/, indent_str) for s in content.split('\n')).join('\n')}\n"
					"<#{tag}#{attrstr}>#{content}</#{tag}>"

		(label) ->
			if label instanceof Array
				expand_content(label)
			else
				curry (args..., content) ->
					[args..., content] = args if not content?
					tag2str(label, (accumulate {}, args, extend), content)

module.exports =
	jady: jady
	$: close_self

##################################################################

if require.main == module
	{jady, $} = module.exports
	j = jady()

	list_data = [1, 2]

	console.log 'RESULT:\n' + (
		j('html') [
			j('head') [
				j('title') 'a coffee-jady demo'
			]
			j('body') [
				j('h1') 'this is a calculator'
				j('div') [
					j('input#input_1.btn')(type: 'number', ngModel: 'input_value')
					j('input')(id: 'input_2', type: 'number')
					j('div') [
						j('button')(id: 'btn', class: 'red notice') 'calculate'
						j('button')(id: 'btn', class: {blue: true, notice: false}) 'clear'
					]
				]
				j('bbr')()
				j('bbr')
				j('br')
				j('bbr') $
				for x in list_data
					for y in list_data
						j('label') "#{x}, #{y}"
				if false
					j('label') 3
				if true then [
					j('label#lab') 4
					j('label#lab') 5
				]
			]
		]
	) #.replace /\n/g, '$'

	#log -> j('abc')
