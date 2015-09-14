import com.github._38.radiation.ast._

object Main {
	def main(args:Array[String]) {
		import org.mozilla.javascript.Parser
		val ra = (new Parser()).parse("""f(1,2,3);
if(x == 2)
	f();
else
	g();
if(x)
	k();
if(x) {
	foo();
	-goo();
}
else koo()++;
{}
x.y.z.f;
f[3] = f(2)
do {
	x ++
	continue
} while(x < 10)
f()?g():k();
[1, 2, 3]
for(i = 0; i < 10 ; i ++)
	continue;

f((2 + 3) * 5)

a = /test/

'test\\n' + 2

a = {
	"x":2,
	"y":3
}
try {
	foo();
} catch (e if e.isInstanceOf(k)) {
	console.print(e)
} catch(e) {
	die();
} finally {
	close();
}

switch(x) {
	case 0: foo();
	case 1: goo();
	default: koo();
}
var a = 2, b, c = a + 1;
for(var a = 3; a < 5; a ++) for(var b = 2; b < 5; b ++);
const x = 3;
function foo() {
	return x
}
(function (a,b) { return a + b;})()
""", "test.js", 0)
		val node:Node = ra
		System.out.println(node)
		System.out.println(node.targetCode)
		//val Program(ExprStmt(Call(_, Arguments(first :: _))) :: _) = node
		//System.out.println(first)
	}
}
