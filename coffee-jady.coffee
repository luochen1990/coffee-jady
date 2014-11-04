require('coffee-mate/global')

jady = do ->
	default_opts = require('./default_opts')

	type = (x) ->
		t_x = typeof x
		if not x? # null or undefined
			'null'
		else if t_x isnt 'object'
			t_x # 'string' or 'number'
		else if x instanceof Array
			'array'
		else if x instanceof Function
			'function'
		else
			'object'

	is_content = (arg) ->
		type(arg) != 'object'

	curry = (callback) ->
		got = []
		rec = (arg) ->
			got.push(arg)
			if is_content(arg)
				got.push(null) if not is_content(got.last)
				return callback(got...)
			else
				return rec
		rec

	(opts = {}) ->
		opts = Object.extend({}, opts, default_opts)

		indent_str = if typeof(opts.indent) == 'number' then ' '.repeat(opts.indent) else opts.indent ? '\t'

		doctype_line = opts.doctype_line ? opts.doctypes(opts.doctype)
		self_closing = if opts.doctype == 'html' then opts.self_closing else (-> false)

		extract_tag = do ->
			snake = (s) ->
				s.replace(/[A-Z]/g, (c) -> "-#{c}").toLowerCase()

			(label, args, content) ->
				attr = Object.extend({}, args...) #(accumulate {}, args, extend)
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
				if type(attr.style) == 'object'
					attr.style = ("#{k}:#{v}" for k, v of attr.style).join(', ')
				attr = Object.extend (id: (id ? attr.id), class: classes.join(' ') or null), attr
				attrstr = ((if bool(v) then " #{snake(k)}" else " #{snake(k)}=\"#{v}\"") for k, v of attr when v? and (not bool(v)? or bool(v))).join('')

				if self_closing(tag)
					if opts.doctype == 'html'
						["<#{tag}#{attrstr}>"]
					else
						["<#{tag}#{attrstr} />"]
				else
					["<#{tag}#{attrstr}>", "</#{tag}>"]

		output = opts.output ? do ->
			result = ''
			write: (s) -> result += s
			read: -> result

		indent = 0

		write_line = (s) -> output.write('\n' + indent_str.repeat(indent) + s)

		j = do ->
			expand = do ->
				(begin_tag, content, end_tag) ->
					if type(content) == 'function'
						write_line begin_tag
						indent += 1
						do content
						indent -= 1
						write_line end_tag
					else
						#log -> [begin_tag, content, end_tag]
						write_line begin_tag + (content ? '') + (end_tag ? '')

			(label, args..., content) ->
				if type(content) == 'object'
					args.push content
					content = null
				[begin_tag, end_tag] = extract_tag(label, args, content)
				expand(begin_tag, content, end_tag)

		Object.defineProperties j,
			text:
				value: (s) -> write_line s.replace('<', '&lt')
			raw:
				value: (s) -> write_line s

		(callback) ->
			callback(j)
			if opts.output?
				null
			else
				if opts.with_doctype
					doctype_line + '\n' + output.read()
				else
					output.read()

module.exports = jady

##################################################################

if module? and require?.main == module
	jady = module.exports

	list_data = [1, 2]

	html = jady(doctype: 'html', with_doctype: true) (j) ->
		j 'html', ->
			j 'head', ->
				j 'title', 'a coffee-jady demo'
			j 'body', ->
				j 'h1', 'this is a calculator'
				j 'div', ngRepeat: 'x in ls', style: {width: '100px', height: '1em'}, ->
					j 'input#input-1.btn', type: 'number', ngModel: 'input-value'
					j 'input', id: 'input-2', type: 'number'
					j 'div', ->
						j 'button', id: 'btn', class: 'red notice', 'calculate'
						j 'button', id: 'btn', class: {blue: true, notice: false}, 'clear'
				j 'br'
				j 'bbr'
				j 'bbr', ''
				j 'bbr', ->
				for x in list_data
					for y in list_data
						j 'div', ->
							j 'label', "#{x}, #{y}"
				if false
					j 'label.if', 3
				if true
					j 'label.if', 4
					j 'label.if', 5

	console.log 'entire:\n' + (html) #.replace /\n/g, '$'

	console.log 'fragment:\n' + jady() (j) ->
		j 'abc', ->
			j.raw '<hi>'
			j 'br'
			j.text '<hello>'

