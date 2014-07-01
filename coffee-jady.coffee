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

String::format = (args) ->
	this.replace /\{(\w+)\}/g, (m, i) -> if args[i]? then args[i] else m

String::repeat = (n) ->
	[r, pat] = ['', this]
	while n > 0
		r += pat if n & 1
		n >>= 1
		pat += pat
	r

Object.defineProperties Array.prototype,
    first:
        get: -> this[0]
        set: (v) -> this[0] = v
    last:
        get: -> this[@length - 1]
        set: (v) -> this[@length - 1] = v

close_self = new Object

jady = do ->
	(opts = {}) ->
		doctype = opts.doctype ? 'html'
		indent_str = if typeof(opts.indent) == 'number' then ' '.repeat(opts.indent) else opts.indent ? '\t'

		join_inline = opts.join_inline ? do ->
			ls = ['a', 'abbr', 'acronym', 'b', 'br', 'code', 'em', 'font', 'i', 'img', 'ins', 'kbd', 'map', 'samp', 'small', 'span', 'strong', 'sub', 'sup']
			is_inline_tag = new RegExp("^(#{ls.join('|')})$")
			(tag) ->
				is_inline_tag.test(tag)

		self_closing = opts.self_closing ? do ->
			ls = ['area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'keygen', 'link', 'menuitem', 'meta', 'param', 'source', 'track', 'wbr']
			is_self_closing_tag = new RegExp("^(#{ls.join('|')})$")
			(tag) ->
				is_self_closing_tag.test(tag)

		doctypes = opts.doctypes ? do ->
			d =
				'default': '<!DOCTYPE html>'
				'xml': '<?xml version="1.0" encoding="utf-8" ?>'
				'transitional': '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'
				'strict': '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'
				'frameset': '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd">'
				'1.1': '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">'
				'basic': '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Basic 1.1//EN" "http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd">'
				'mobile': '<!DOCTYPE html PUBLIC "-//WAPFORUM//DTD XHTML Mobile 1.2//EN" "http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd">'
			(doctype) ->
				d[doctype]

		doctype_line = if opts.doctype_line isnt undefined then opts.doctype_line else doctypes(doctype)

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

		extract_tag = do ->
			snake = (s) ->
				s.replace(/[A-Z]/g, (c) -> "-#{c}").toLowerCase()

			(label, args, content) ->
				attr = (accumulate {}, args, extend)
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
					["<#{tag}#{attrstr}>"]
				else
					["<#{tag}#{attrstr}>", "</#{tag}>"]

		output = do ->
			result = ''
			write: (s) -> result += s
			result: -> result

		j = do ->
			expand = do ->
				indent = 0
				(begin_tag, content, end_tag) ->
					if type(content) == 'function'
						output.write '\n' + indent_str.repeat(indent) + begin_tag
						indent += 1
						do content
						indent -= 1
						output.write '\n' + indent_str.repeat(indent) + end_tag
					else
						#log -> [begin_tag, end_tag, content]
						output.write '\n' + indent_str.repeat(indent) + begin_tag + (if content? and content isnt close_self then content else '') + (end_tag ? '')

			(label) ->
				curry (args..., content) ->
					[begin_tag, end_tag] = extract_tag(label, args, content)
					expand(begin_tag, content, end_tag)

		Object.defineProperties j,
			result:
				get: output.result

		return j

module.exports =
	jady: jady
	$: close_self

##################################################################

if require.main == module
	{jady, $} = module.exports

	list_data = [1, 2]

	j = jady doctype: 'html'
	j('html') ->
		j('head') ->
			j('title') 'a coffee-jady demo'
		j('body') ->
			j('h1') 'this is a calculator'
			j('div') ->
				j('input#input-1.btn')(type: 'number', ngModel: 'input-value')()
				j('input')(id: 'input-2', type: 'number')()
				j('div') ->
					j('button')(id: 'btn', class: 'red notice') 'calculate'
					j('button')(id: 'btn', class: {blue: true, notice: false}) 'clear'
			j('br')()
			j('bbr') $
			for x in list_data
				for y in list_data
					j('label') "#{x}, #{y}"
			if false
				j('label.if') 3
			if true
				j('label.if') 4
				j('label.if') 5

	console.log 'RESULT:\n' + (j.result) #.replace /\n/g, '$'

	#log -> j('abc')
