using System;namespace Taste {public enum Op { // opcodes	ADD, SUB, MUL, DIV, EQU, NEQ, LSS, LSE, GTR, GTE, NEG,	LOAD, LOADG, STO, STOG, STOA, STOGA, CONST,	CALL, RET, ENTER, LEAVE, JMP, FJMP, READ, WRITE, WRITEC, WRITEL, WRITES, STR}public class CodeGenerator {		string[] opcode =	  {"ADD  ", "SUB  ", "MUL  ", "DIV  ", "EQU  ", "NEQ  ", "LSS  ", "LSE  ", "GTR  ", "GTE  ", "NEG  ",	   "LOAD ", "LOADG", "STO  ", "STOG ", "STOA", "STOGA", "CONST", "CALL ", "RET  ", "ENTER",	   "LEAVE", "JMP  ", "FJMP ", "READ ", "WRITE", "WRITEC", "WRITEL", "WRITES ", "STR"};	public int progStart;	// address of first instruction of main program	public int pc;				// program counter	byte[] code = new byte[3000];	// data for Interpret	int[] globals = new int[100];	int[] stack = new int[100];	string[] strStack = new string[100];	int top;	// top of stack	int bp;		// base pointer	int sIndex;	public CodeGenerator() {		pc = 1; progStart = -1;	}	//----- code generation methods -----		public void Put(int x) {		code[pc++] = (byte)x;	}		public void Emit (Op op) {		Put((int)op);	}	public void Emit (Op op, int val) {		Emit(op); Put(val>>8); Put(val);	}		public void Emit(Op op, int val, int val2){		Emit(op, val); Put(val2>>8); Put(val2);	}		public void Emit (Op op, string str, int len) {		Emit(op); Put(len>>8); Put(len); 				byte[] temp = StrToByteArray(str);		for(int i=0; i<temp.Length; i++){			Put(temp[i]);		}	}	public void Patch (int adr, int val) {		code[adr] = (byte)(val>>8); code[adr+1] = (byte)val;	}	public void Decode() {		int maxPc = pc;		pc = 1;		while (pc < maxPc) {			Op code = (Op)Next();			Console.Write("{0,3}: {1} ", pc-1, opcode[(int)code]);			switch(code) {				case Op.LOAD: case Op.LOADG: case Op.CONST: case Op.STO: case Op.STOG: 				case Op.STOA: case Op.STOGA: case Op.CALL: case Op.ENTER: case Op.JMP: case Op.FJMP:					Console.WriteLine(Next2()); break;				case Op.ADD: case Op.SUB: case Op.MUL: case Op.DIV: case Op.NEG:				case Op.EQU: case Op.NEQ: case Op.LSS: case Op.LSE: case Op.GTR: case Op.GTE: case Op.RET: case Op.LEAVE: 				case Op.READ: case Op.WRITE: case Op.WRITEC: case Op.WRITEL: case Op.WRITES:					Console.WriteLine(); break;				case Op.STR:					int i = Next2();					Console.Write(i+" ");					Console.WriteLine(NextS(i));					break;			}		}	}	//----- interpreter methods -----		int Next () {		return code[pc++];	}	int Next2 () {		int x,y; 		x = (sbyte)code[pc++]; y = code[pc++];		return (x << 8) + y;	}		string NextS(int len)  {		byte[] r = new byte[len];				for(int i=0; i<len; i++){			r[i] = code[pc++];		}		return ByteArrayToStr(r);	}	int Int (bool b) {		if (b) return 1; else return 0;	}	void Push (int val) {		stack[top++] = val;	}		void PushS (string str) {		strStack[sIndex++] = str;	}	int Pop() {		return stack[--top];	}	int ReadInt(FileStream s) {		int ch, sign;		do {ch = s.ReadByte();} while (!(ch >= '0' && ch <= '9' || ch == '-'));		if (ch == '-') {sign = -1; ch = s.ReadByte();} else sign = 1;		int n = 0;		while (ch >= '0' && ch <= '9') {			n = 10 * n + (ch - '0');			ch = s.ReadByte();		}		return n * sign;	}		public void Interpret (string data) { 		int val;		try {			FileStream s = new FileStream(data, FileMode.Open);			Console.WriteLine();            pc = 1; stack[0] = 0; top = 1; bp = 0; sIndex = 0;						int len, adr, i, size;			string str;			for(Op globalO = (Op)Next(); globalO != Op.ENTER; globalO = (Op)Next()){				switch (globalO) {					case Op.CONST: Push(Next2()); break;					case Op.LOADG: Push(globals[Next2()]); break;					case Op.STOG:  globals[Next2()] = Pop(); break;					case Op.STOGA: 						adr = Next2();						size = Next2();						for(i = 0; i<size; i++){							globals[adr+i] = 0;						} 						break;					case Op.ADD:   Push(Pop()+Pop()); break;					case Op.SUB:   Push(-Pop()+Pop()); break;					case Op.DIV:   val = Pop(); Push(Pop()/val); break;					case Op.MUL:   Push(Pop()*Pop()); break;					case Op.NEG:   Push(-Pop()); break;					case Op.EQU:   Push(Int(Pop()==Pop())); break;					case Op.NEQ:   Push(Int(Pop()!=Pop())); break;					case Op.LSS:   Push(Int(Pop()>Pop())); break;					case Op.LSE:   Push(Int(Pop()>=Pop())); break;					case Op.GTR:   Push(Int(Pop()<Pop())); break;					case Op.GTE:   Push(Int(Pop()<=Pop())); break;					case Op.READ:  val = ReadInt(s); Push(val); break;					case Op.STR:   						Push(sIndex);						len = Next2();						str = NextS(len);						PushS(str); break;					default:    throw new Exception("illegal opcode");				}			}						pc = progStart;			for (;;) {				switch ((Op)Next()) {					case Op.CONST: Push(Next2()); break;					case Op.LOAD:  Push(stack[bp+Next2()]); break;					case Op.LOADG: Push(globals[Next2()]); break;					case Op.STO:   stack[bp+Next2()] = Pop(); break;					case Op.STOA:						adr = Next2(); size = Next2();						for(i = 0; i<size; i++){							stack[bp+adr+i] = 0;						}						break;					case Op.STOG:  globals[Next2()] = Pop(); break;					case Op.STOGA: 						adr = Next2(); size = Next2();						for(i = 0; i<size; i++){							globals[adr+i] = 0;						} 						break;					case Op.ADD:   Push(Pop()+Pop()); break;					case Op.SUB:   Push(-Pop()+Pop()); break;					case Op.DIV:   val = Pop(); Push(Pop()/val); break;					case Op.MUL:   Push(Pop()*Pop()); break;					case Op.NEG:   Push(-Pop()); break;					case Op.EQU:   Push(Int(Pop()==Pop())); break;					case Op.NEQ:   Push(Int(Pop()!=Pop())); break;					case Op.LSS:   Push(Int(Pop()>Pop())); break;					case Op.LSE:   Push(Int(Pop()>=Pop())); break;					case Op.GTR:   Push(Int(Pop()<Pop())); break;					case Op.GTE:   Push(Int(Pop()<=Pop())); break;					case Op.JMP:   pc = Next2(); break;					case Op.FJMP:  val = Next2(); if (Pop()==0) pc = val; break;					case Op.READ:  val = ReadInt(s); Push(val); break;					case Op.WRITE: Console.Write(Pop()); break;					case Op.WRITEC: Console.Write(", "); break;					case Op.WRITEL: Console.WriteLine(); break;					case Op.WRITES: 						str = strStack[Pop()];						Console.Write(str); break;					case Op.CALL:  Push(pc+2); pc = Next2(); break;					case Op.RET:   pc = Pop(); if (pc == 0) return; break;					case Op.ENTER: Push(bp); bp = top; top = top + Next2(); break;					case Op.LEAVE: top = bp; bp = Pop(); break;					case Op.STR:   						Push(sIndex);						len = Next2();						str = NextS(len);						PushS(str); break;					default:    throw new Exception("illegal opcode");				}			}		} catch (IOException) {			Console.WriteLine("--- Error accessing file {0}", data);			System.Environment.Exit(0);		}	}	private static byte[] StrToByteArray(string str) {		System.Text.ASCIIEncoding encoding = new System.Text.ASCIIEncoding();		return encoding.GetBytes(str);	}		private static string ByteArrayToStr (byte[] b){		System.Text.ASCIIEncoding encoding = new System.Text.ASCIIEncoding();		return encoding.GetString(b);	}} // end CodeGenerator} // end namespace