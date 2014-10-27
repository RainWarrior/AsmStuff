package asmstuff.mixin;

interface I {
	public int x();

	public int y(int z, String t);

	public void z(int z, float t);
}

class I$MixinImpl {
	public static int y(A that, int z, String t) {
		return 3;
	}

	public static void z(A that, int z, float t) {
		return;
	}
}

class A implements I {
	public int x() {
		return 4;
	}

	@Mixin.Dummy
	@Override
	public int y(int z, String t) { return 0; }
	/*public int y(int z, String t) {
		return IImpl.y(this, z, t);
	}*/

	@Mixin.Dummy
	@Override
	public void z(int z, float t) {}
	/*public void z(int z, float t) {
		IImpl.z(this, z, t);
	}*/
}

public class MixinTest {
	public static void main(String[] args) {
		A a = new A();
		I i = a;
		System.out.println(i.y(1, ""));
	}
}
