//Programa 1 - Testando if/else(Com e sem o break)(Com continue)
/*
var x = 5, y = 10, z;
x + y;
if( x > y) {
	z = y;
} else {
	if(x == y) {
		z = x;
	} else {
		break;
		z = 50;
	}
	continue;
	x = 20;
}
x;
*/


//Programa 2 - - Testando  while e for
/*
var x = 0, y = 10;
x + y;
if (x == 1) {
	var z = true;
}
else {
	var x = 10;
}
while (x < 15)  {
	x = x+1;
	for (; x < 20;) {
		x = x+1
	}
}
x;
*/

//Programa 3 - Testando Do while com for (Sem inicializar e sem Incremento)
/*
var x = 0, y = 10;
x + y;
if (x == 1) {
	var z = true;
}
else {
	var x = 10;
}
do {
	x = x+1;
	for (; x < 20;) {
		x = x+1
	}
} while (x < 30);
x;
*/

//Programa 4 - Testando o for
/*
var x = 10;
for (var i = 0; i < 50; i = i + 1) {
		x = x + i;
}
x;
*/

//Programa 5 - Testando break

/*
var x = 20;
for (var i = 0;  ; i = i + 1){
	break;
	x = 30;
}
if (x == 20) {
	break;
	x = 30;
}
*/

//Programa 6 - Testando Listas
/*
var lista1 = [1, 2, 3];
var lista2 = [3*2, 4 + 2, 1 + 3];

var a1 = head(lista1);
var a2 = tail(lista1);
var a3 = tail(lista2);

var a4 = concat(a2, a3);
a4;
*/

//Programa 8 - Testando funcao
/*
function soma2(a) {
	var abc = a+2;
	return abc;
}
soma2(5);
*/


//Programa 9 - Testando fucao recursiva
/*
function fibonacci(num) {
  if (num <= 1)
	{
		return 1;
	}
	else {
		return fibonacci(num - 1) + fibonacci(num - 2);
	}
}

fibonacci(10);
*/


//Programa 10
// Length function

function len(A){
	if (A == []){
		return 0;
	}
	else {
		return 1 + len(tail(A))
	}
}

len([1,2,3,4,5,6,7,8,9,0]);
