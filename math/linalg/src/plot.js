(function(){

	var set_option = function(obj, prop, val) {
		if(!obj.hasOwnProperty(prop))
			obj[prop] = val;
	};

	clone_options = function(obj) {
		var options = {};
		for(var key in obj) {
			options[key] = obj[key];
		}
		return options;
	}


	
	// MATRICES

	var Matrix = function(rows, cols, elems) {
		this.rows = rows;
		this.columns = cols;
		this.elements = elems === undefined ? new Array(rows*cols) : elems;
	};

	Matrix.prototype.set_value = function(row, col, val) {
		this.elements[row * this.columns + col] = val;
	};

	Matrix.prototype.get_value = function(row, col) {
		return this.elements[row * this.columns + col];
	};

	Matrix.prototype.product = function(A) {
		if(this.columns !== A.rows)
			return null;
		var M = new Matrix(this.rows, A.columns);
		for(var i = 0; i < this.rows; i++) {
			for(var j = 0; j < A.columns; j++) {
				var val = 0;
				for(var k = 0; k < this.columns; k++) {
					val += this.get_value(i, k) * A.get_value(k, j);
				}
				M.set_value(i, j, val);
			}
		}
		return M;
	};

	Matrix.prototype.scalar = function(scalar) {
		return new Matrix(this.rows, this.columns, this.elements.map(function(x){
			return x*scalar;
		}));
	};

	Matrix.prototype.to_vector = function(options) {
		return new Vector(this.elements[0], this.elements[1], options);
	};



	// VECTORS

	var Vector = function(x, y, options) {
		options = options === undefined ? {} : options;
		set_option(options, "color", "#ff6f55");
		set_option(options, "tail", true);
		set_option(options, "tail_width", 2);
		set_option(options, "tail_color", options.color);
		set_option(options, "head", true);
		set_option(options, "head_angle", 30);
		set_option(options, "head_color", options.color);
		set_option(options, "head_form", "triangle");
		set_option(options, "head_width", options.head_form === "triangle" ? 13 : 5);
		set_option(options, "label", "");
		set_option(options, "label_color", options.color);
		set_option(options, "label_size", 22);
		set_option(options, "label_arrow", true);
		set_option(options, "origin", null);
		this.x = x;
		this.y = y;
		this.options = options;
	};

	Vector.prototype.to_matrix = function() {
		return new Matrix(2, 1, [this.x, this.y]);
	};

	Vector.prototype.draw = function(plot) {
		if(plot.options.canvas === null)
			return false;
		var ctx = plot.options.canvas.getContext("2d");
		var unit_size = plot.options.unit_size;
		var center_x = plot.options.width/2;
		var center_y = plot.options.height/2;
		var origin_x = this.options.origin === null ? 0 : this.options.origin.x;
		var origin_y = this.options.origin === null ? 0 : this.options.origin.y;
		var head_x = center_x+unit_size*(origin_x+this.x);
		var head_y = center_y-unit_size*(origin_y+this.y);
		// Tail
		if(this.options.tail === true) {
			ctx.lineWidth = this.options.tail_width;
			ctx.strokeStyle = this.options.tail_color.toString();
			ctx.beginPath();
			ctx.moveTo(center_x+unit_size*origin_x, center_y-unit_size*origin_y);
			ctx.lineTo(head_x, head_y);
			ctx.closePath();
			ctx.stroke();
		}
		// Label
		if(this.options.label !== "") {
			var text_x = center_x+unit_size*origin_x+unit_size*this.x/2 + (Math.sign(this.x-origin_x) === Math.sign(this.y-origin_y) ? 5 : -5);
			var text_y = center_y-unit_size*origin_y-unit_size*this.y/2 + 10;
			ctx.fillStyle = this.options.label_color.toString();
			ctx.strokeStyle = this.options.label_color.toString();
			ctx.font = "bold " + this.options.label_size + "px verdana";
			ctx.textAlign = Math.sign(this.x-origin_x) === Math.sign(this.y-origin_y) ? "left" : "right";
			ctx.textBaseline = "top"; 
			ctx.fillText(this.options.label, text_x, text_y);
			// Arrow
			if(this.options.label_arrow === true) {
				var text_measure = ctx.measureText(this.options.label);
				var text_origin = ctx.textAlign === "left" ? text_x : text_x-text_measure.width;
				var text_ending = ctx.textAlign === "left" ? text_x+text_measure.width : text_x;
				ctx.lineWidth = 1;
				ctx.beginPath();
				ctx.moveTo(text_origin, text_y-1);
				ctx.lineTo(text_ending, text_y-1);
				ctx.lineTo(text_ending-3, text_y-4);
				ctx.lineTo(text_ending-3, text_y+2);
				ctx.lineTo(text_ending, text_y-1);
				ctx.closePath();
				ctx.fill();
				ctx.stroke();
			}
		}
		// Head
		if(this.options.head === true) {
			var head_width = this.options.head_width;
			ctx.lineWidth = 0;
			ctx.fillStyle = this.options.head_color.toString();
			switch(this.options.head_form) {
				case "triangle":
					var head_angle = this.options.head_angle;
					var angle = Math.atan(this.y/this.x);
					var dir = this.x < 0 ? 1 : -1;
					ctx.beginPath();
					ctx.moveTo(head_x, head_y);
					ctx.lineTo(
						head_x+dir*head_width*Math.cos(angle-head_angle*Math.PI/180),
						head_y-dir*head_width*Math.sin(angle-head_angle*Math.PI/180));
					ctx.lineTo(
						head_x+dir*head_width*Math.cos(angle+head_angle*Math.PI/180),
						head_y-dir*head_width*Math.sin(angle+head_angle*Math.PI/180));
					ctx.closePath();
					ctx.fill();
					break;
				case "circle":
				ctx.moveTo(head_x, head_y);
					ctx.beginPath();
					ctx.arc(head_x, head_y, head_width, 0, 2*Math.PI);
					ctx.closePath();
					ctx.fill();
					break;
			}
		}
		return true;
	}



	// PLOTS

	var Plot = function(options) {
		options = options === undefined ? {} : options;
		set_option(options, "canvas", null);
		set_option(options, "width", 300);
		set_option(options, "height", 300);
		set_option(options, "axis", true);
		set_option(options, "background", true);
		set_option(options, "background_color", "#111111");
		set_option(options, "axis_color", "#cfcfcf");
		set_option(options, "axis_width", 2);
		set_option(options, "grid", true);
		set_option(options, "grid_color", "#333333");
		set_option(options, "grid_width", 1);
		set_option(options, "grid_size", 25);
		set_option(options, "unit_size", 25);
		this.options = options;
		this.vectors = [];
	};

	Plot.prototype.generate_uniform = function(options) {
		var size_x = this.options.width/this.options.unit_size;
		var size_y = this.options.height/this.options.unit_size;
		var n = Math.max(Math.floor(size_x/2), Math.floor(size_y/2));
		var total = (n+1)*(n+1);
		for(var i = -n; i <= n; i ++) {
			for(var j = -n; j <= n; j++) {
				var opt = clone_options(options);
				opt.color = "hsl(" + (360*((n+1)*i+j)/total) + ",100%, 50%)";
				this.add_vector(new Vector(i, j, opt));
			}
		}
	};

	Plot.prototype.add_vector = function(v) {
		this.vectors.push(v);
	};

	Plot.prototype.refresh = function() {
		if(this.options.canvas === null)
			return false;
		var ctx = this.options.canvas.getContext("2d");
		// Background
		if(this.options.background === true) {
			ctx.fillStyle = this.options.background_color.toString();
			ctx.beginPath();
			ctx.rect(0, 0, this.options.width, this.options.height);
			ctx.closePath();
			ctx.fill();
		}
		// Grid
		if(this.options.grid === true) {
			var grid_size = this.options.grid_size;
			var init_x = this.options.width/2 - grid_size * Math.floor(this.options.width/2/grid_size);
			var init_y = this.options.height/2 - grid_size * Math.floor(this.options.height/2/grid_size);
			ctx.strokeStyle = this.options.grid_color.toString();
			ctx.lineWidth = this.options.grid_width;
			for(var i = init_x; i < this.options.width; i += grid_size) {
				ctx.beginPath();
				ctx.moveTo(i, 0);
				ctx.lineTo(i, this.options.height);
				ctx.closePath();
				ctx.stroke();
			}
			for(var j = init_y; j < this.options.height; j += grid_size) {
				ctx.beginPath();
				ctx.moveTo(0, j);
				ctx.lineTo(this.options.width, j);
				ctx.closePath();
				ctx.stroke();
			}
		}
		// Axis
		if(this.options.axis === true) {
			ctx.strokeStyle = this.options.axis_color.toString();
			ctx.lineWidth = this.options.axis_width;
			ctx.beginPath();
			ctx.moveTo(this.options.width/2, 0);
			ctx.lineTo(this.options.width/2, this.options.height);
			ctx.closePath();
			ctx.stroke();
			ctx.beginPath();
			ctx.moveTo(0, this.options.height/2);
			ctx.lineTo(this.options.width, this.options.height/2);
			ctx.closePath();
			ctx.stroke();
		}
		// Vectors
		for(var i = 0; i < this.vectors.length; i++)
			this.vectors[i].draw(this);
		return true;
	};

	Plot.prototype.transform = function(T) {
		for(var i = 0; i < this.vectors.length; i++) {
			var vector = T.product(this.vectors[i].to_matrix());
			this.vectors[i].x = vector.get_value(0, 0);
			this.vectors[i].y = vector.get_value(1, 0);
		}
	};



	// GLOBAL
	window.Matrix = Matrix;
	window.Vector = Vector;
	window.Plot = Plot;

})();