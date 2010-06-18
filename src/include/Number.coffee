class Number
	@MAX_VALUE: `new CSNumber(java.util.Double.MAX_VALUE)`
	@MIN_VALUE: `new CSNumber(java.util.Double.MAX_VALUE)`
	@NaN: `new CSNumber(java.util.Double.NaN)`
	@NEGATIVE_INFINITY: `new CSNumber(java.util.Double.NEGATIVE_INFINITY)`
	@POSITIVE_INFINITY: `new CSNumber(java.util.Double.POSITIVE_INFINITY)`
	
	toString: ->
		`return new CSString(java.util.Double.toString(this.value));`
	toLocaleString: ->
		`return new CSString(java.util.Double.toString(this.value));`
	valueOf: ->
		`return new CSNumber(this.value);`
	toFixed: -> null
		// TODO
	toExponential: -> null
		// TODO
	toPrecision: -> null
		// TODO
	