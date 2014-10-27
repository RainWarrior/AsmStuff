package asmstuff.mixin;

import java.lang.Iterable;
import java.util.Arrays;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.ClassNode;

import asmstuff.MethodTree;
import asmstuff.Types.MethodAcc;

import scala.collection.JavaConversions;

public class Mixer extends ClassVisitor {
	MethodTree methodTree;
	String name;

	public Mixer(ClassVisitor cv, MethodTree methodTree) {
		super(Opcodes.ASM4, cv);
		this.methodTree = methodTree;
	}

	@Override
	public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
		this.name = name;
		super.visit(version, access, name, signature, superName, interfaces);
	}

	@Override
	public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
		// replace dummy methods with real ones
		return new MethodTransformer(cv.visitMethod(access, name, desc, signature, exceptions), new MethodAcc(this.name, name, desc, access));
	}

	@Override
	public void visitEnd() {
		Iterable<MethodAcc> ms = JavaConversions.asJavaIterable(methodTree.propagatedMMap().apply(this.name));
		outer:
		for(MethodAcc m : ms) {
			if(!m.owner().equals(name) && (m.access() & Opcodes.ACC_ABSTRACT) != 0) {
				// found abstract method, see if it's overriden. Can be O(N), but lazy (and probably premature optimization), so O(N^2) :P
				for(MethodAcc m2 : ms) {
					//System.out.println("! " + m2.toString() + " " + m2.equals(m) + " " + ((m2.access() & Opcodes.ACC_ABSTRACT) == 0) + " " + m2.overrides(m));
					if(!m2.equals(m) && (m2.access() & Opcodes.ACC_ABSTRACT) == 0 && m2.overrides(m)) continue outer;
				}
				// It's abstract, and not overriden => it came from mixin
				//System.out.println("Found abstract method: " + name + " " + m.owner() + "/" + m.name() + m.desc());
				MethodVisitor mv = cv.visitMethod(m.access(), m.name(), m.desc(), null, null);
				if(mv != null) generateMixinMethod(m, mv);
			}
		}
		cv.visitEnd();
	}

	public void generateMixinMethod(MethodAcc m, MethodVisitor mv) {
		//System.out.println("generating " + m.owner() + "/" + m.name() + m.desc());
		mv.visitCode();
		mv.visitVarInsn(Opcodes.ALOAD, 0); // this
		Type[] args = Type.getArgumentTypes(m.desc());
		Type ret = Type.getReturnType(m.desc());
		for(int i = 0; i < args.length; i++) {
			mv.visitVarInsn(args[i].getOpcode(Opcodes.ILOAD), i + 1);
		}
		// needs resolving
		String mixinName = m.owner() + "$MixinImpl";
		//System.out.println(m.owner() + " " + methodTree.getMethods(mixinName).size());
		for(MethodAcc m2 : JavaConversions.asJavaIterable(methodTree.getMethods(mixinName))) {
			//System.out.println("trying " + m2.name() + m2.desc());
			if(!m2.name().equals(m.name()) || (m2.access() & Opcodes.ACC_STATIC) == 0) continue;
			//System.out.println(1);
			Type[] args2 = Type.getArgumentTypes(m2.desc());
			if(args2.length != args.length + 1) continue;
			Type[] cmp = new Type[args.length];
			for(int i = 0; i < args.length; i++) cmp[i] = args2[i + 1];
			if(Arrays.deepEquals(args, cmp)) {
				// found the method we need to call
				mv.visitMethodInsn(Opcodes.INVOKESTATIC, mixinName, m2.name(), m2.desc());
				mv.visitInsn(ret.getOpcode(Opcodes.IRETURN));
				mv.visitMaxs(args.length + 1, args.length + 1);
				mv.visitEnd();
				return;
			}
		}
		// didn't find any
		//System.out.println(3);
		throw new RuntimeException("Couldn't find mixin method for " + m.owner() + "/" + m.name() + m.desc());
	}

	public class MethodTransformer extends MethodVisitor {
		MethodAcc m;
		boolean dummy = false;

		public MethodTransformer(MethodVisitor mv, MethodAcc m) {
			super(Opcodes.ASM4, mv);
			this.m = m;
		}

		@Override public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
			if(desc.equals(Type.getDescriptor(Mixin.Dummy.class))) dummy = true;
			return super.visitAnnotation(desc, visible);
		}

		@Override public void visitCode() {
			if(dummy) {
				//System.out.println("found dummy: " + m.owner() + "/" + m.name() + m.desc());
				// find actual abstract method this dummy overrides
				for(MethodAcc m2 : JavaConversions.asJavaIterable(methodTree.propagatedMMap().apply(m.owner()))) {
					//System.out.println("trying prototype: " + m2.owner() + "/" + m2.name() + m2.desc());
					if(!m.equals(m2) && m.overrides(m2) && (m2.access() & Opcodes.ACC_ABSTRACT) != 0) {
						if(mv != null) generateMixinMethod(m2, mv);
						this.mv = null; // ignore original code (rest of the events
						return;
					}
				}
				// not found
				throw new RuntimeException("Couldn't find prototype for dummy " + m.owner() + "/" + m.name() + m.desc());
			}
			else {
				super.visitCode();
			}
		}
	}
}
