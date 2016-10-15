/*
var x = 0, y = 10;
x + y;
*/

//Programa 0 - Testando If single
/*
var a = 2;
if(a>1)
{
	a+10;
}
*/
/*
//Programa 1 - Testando if/else(Com e sem o break)
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

}

*/

//Programa 3 - Testando for
/*
var x = 1, y = 10, p = 0;
x + y;
if (x == 1) {
	p = 100;
	var z = true;
}
else {
	var x = 10;
}
for (; x < 15;) {
	x = x+1
}
*/

//Programa 4 - Testando Do while e for

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



//Programa 6 - Testando  while e for
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

*/

//Programa 7 - Testando Listas
/*
var x = [1,2,3], y=[1,2,3], z = [1,x,"ola", "mundo", true];
if (x==y) {
	x = x.concat(y.head()).concat(z); // [1,2,3] ++ [1] + [1,x,"ola", "mundo", true]
}
x.tail(); //[2,3,1,1,x,"ola","mundo", true]
*/

//Programa 8 - Testando funcao
/*
function soma2(a) {
	var abc = a+2;
	return abc;
}
soma2(5);
*/

//Programa 9 - Testando Listas
/*
var list1 = [1, 2, 3];
var list2 = [2*2, 4 + 1, 3 + 3];

var a = head(list1);
var b = tail(list1);
var c = tail(list2);

var d = concat(b, c);
d;

*/

//Programa 10 - Testando fucao recursiva
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
