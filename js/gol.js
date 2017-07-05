var canvas;
var input_fps;
var display_fps;
var ctx;
var gameLoopControl = 0;
var map = {};
var isMouseDown = false;
var fps = 15;

function eventMouseUp(e) {
    var x;
    var y;
    if (e.pageX || e.pageY) { 
      x = e.pageX;
      y = e.pageY;
    } else { 
      x = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft; 
      y = e.clientY + document.body.scrollTop + document.documentElement.scrollTop; 
    } 
    x -= canvas.offsetLeft;
    y -= canvas.offsetTop;

    map.board[Math.floor(y/10)][Math.floor(x/10)] ^= 1;
    draw();
	isMouseDown = false;
}

function eventMouseMove(e) {
	if (isMouseDown) {
    	var x;
    	var y;
    	if (e.pageX || e.pageY) { 
    	  x = e.pageX;
    	  y = e.pageY;
    	} else { 
    	  x = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft; 
    	  y = e.clientY + document.body.scrollTop + document.documentElement.scrollTop; 
    	} 
    	x -= canvas.offsetLeft;
    	y -= canvas.offsetTop;
	
    	map.board[Math.floor(y/10)][Math.floor(x/10)] = 1;
    	draw();
	}	
}

function eventMouseDown(e) {
    draw();
	isMouseDown = true;
}

function eventKeyUp(e) {
    var keycode = ('which' in e) ? e.which : e.keyCode;
    switch (keycode) {
    default:break;
    }
}

function eventKeyDown(e) {
    var keycode = ('which' in e) ? e.which : e.keyCode;
    switch (keycode) {
    case 32:
	if (gameLoopControl != 0 || fps == 0) {
	    clearInterval(gameLoopControl);
	    gameLoopControl = 0;
		draw();
	} else {
	    gameLoopControl = setInterval(gameLoop, 1000/fps);
	}
	break;
    default:break;
    }
}

function draw() {
    var x, y;

    y = -1;
    while (++y < map.height) {
	x = -1;
	while (++x < map.width) {
	    if (map.board[y][x]==0)
		ctx.fillStyle = "rgb(0, 0, " + (55 + randInt(0, 200)) + ")";
	    else
		ctx.fillStyle = "rgb(0, " + (205 + randInt(0, 50)) + ", 0)";
	    ctx.fillRect (x*10, y*10, 10, 10);
	}
    }
}

function CORRECT_MAX(i, maxi) {
    return (i < 0 ? maxi-1 : i >= maxi ? 0 : i);
}

function getNeighbours(x, y) {
    var count = 0;
    count+=map.board[CORRECT_MAX(y-1, map.height)][CORRECT_MAX(x-1, map.width)];
    count+=map.board[CORRECT_MAX(y, map.height)][CORRECT_MAX(x-1, map.width)];
    count+=map.board[CORRECT_MAX(y+1, map.height)][CORRECT_MAX(x-1, map.width)];
    count+=map.board[CORRECT_MAX(y-1, map.height)][CORRECT_MAX(x, map.width)];
    count+=map.board[CORRECT_MAX(y+1, map.height)][CORRECT_MAX(x, map.width)];
    count+=map.board[CORRECT_MAX(y-1, map.height)][CORRECT_MAX(x+1, map.width)];
    count+=map.board[CORRECT_MAX(y, map.height)][CORRECT_MAX(x+1, map.width)];
    count+=map.board[CORRECT_MAX(y+1, map.height)][CORRECT_MAX(x+1, map.width)];
    return count;
}

function gameLoop() {
    draw();
    for (var y = 0; y < map.height; ++y) {
	for (var x = 0; x < map.width; ++x) {
	    map.board2[y][x] = getNeighbours(x, y);
	}
    }
    for (var y = 0; y < map.height; ++y) {
	for (var x = 0; x < map.width; ++x) {
	    if (map.board[y][x] == 1) {
		if (map.board2[y][x] <= 1)
		    map.board[y][x] = 0;
		else if (map.board2[y][x] >= 4)
		    map.board[y][x] = 0;
	    } else {
		if (map.board2[y][x] == 3)
		    map.board[y][x] = 1;
	    }
	}
    }
}

function randInt(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
}

function main() {
    input_fps = document.getElementById("id_input_range_fps");
	input_fps.value = fps;
	display_fps = document.getElementById("id_display_fps");
	display_fps.innerHTML = input_fps.value;
	input_fps.addEventListener('input', function(e) {
		fps = input_fps.value;
		display_fps.innerHTML = input_fps.value;
	    clearInterval(gameLoopControl);
		if (fps == 0)
			gameLoopControl = 0;
		else
	    	gameLoopControl = setInterval(gameLoop, 1000/fps);
	});	
    document.addEventListener('keyup', eventKeyUp, true);
    document.addEventListener('keydown', eventKeyDown, true);
    canvas = document.getElementById("canvas");
    canvas.addEventListener('mouseup', eventMouseUp);
    canvas.addEventListener('mousedown', eventMouseDown);
    canvas.addEventListener('mousemove', eventMouseMove);
    ctx = canvas.getContext("2d");
    
    map.width = canvas.width / 10;
    map.height = canvas.height / 10;
    map.board = [];
    map.board2 = [];
    for (var y = 0; y < map.height; ++y) {
	map.board[y] = [];
	map.board2[y] = [];
	for (var x = 0; x < map.width; ++x) {
	    map.board[y][x] = 0;
	    map.board2[y][x] = 0;
	}
    }
    draw();
}
