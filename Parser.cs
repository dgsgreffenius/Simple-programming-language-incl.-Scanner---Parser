
using System;

namespace Taste {



public class Parser {
	public const int _EOF = 0;
	public const int _ident = 1;
	public const int _number = 2;
	public const int _strT = 3;
	public const int _charT = 4;
	public const int maxT = 44;

	const bool T = true;
	const bool x = false;
	const int minErrDist = 2;
	
	public Scanner scanner;
	public Errors  errors;

	public Token t;    // last recognized token
	public Token la;   // lookahead token
	int errDist = minErrDist;

const int // types
	  undef = 0, integer = 1, boolean = 2, str = 3;

	const int // object kinds
	  var = 0, proc = 1, constant = 2;

	public SymbolTable   tab;
	public CodeGenerator gen;
  
/*--------------------------------------------------------------------------*/


	public Parser(Scanner scanner) {
		this.scanner = scanner;
		errors = new Errors();
	}

	void SynErr (int n) {
		if (errDist >= minErrDist) errors.SynErr(la.line, la.col, n);
		errDist = 0;
	}

	public void SemErr (string msg) {
		if (errDist >= minErrDist) errors.SemErr(t.line, t.col, msg);
		errDist = 0;
	}
	
	void Get () {
		for (;;) {
			t = la;
			la = scanner.Scan();
			if (la.kind <= maxT) { ++errDist; break; }

			la = t;
		}
	}
	
	void Expect (int n) {
		if (la.kind==n) Get(); else { SynErr(n); }
	}
	
	bool StartOf (int s) {
		return set[s, la.kind];
	}
	
	void ExpectWeak (int n, int follow) {
		if (la.kind == n) Get();
		else {
			SynErr(n);
			while (!StartOf(follow)) Get();
		}
	}


	bool WeakSeparator(int n, int syFol, int repFol) {
		int kind = la.kind;
		if (kind == n) {Get(); return true;}
		else if (StartOf(repFol)) {return false;}
		else {
			SynErr(n);
			while (!(set[syFol, kind] || set[repFol, kind] || set[0, kind])) {
				Get();
				kind = la.kind;
			}
			return StartOf(syFol);
		}
	}

	
	void AddOp(out Op op) {
		op = Op.ADD; 
		if (la.kind == 5) {
			Get();
		} else if (la.kind == 6) {
			Get();
			op = Op.SUB; 
		} else SynErr(45);
	}

	void ConstDecl() {
		Obj obj; int type; string name; 
		Expect(7);
		Ident(out name);
		Expect(8);
		Expr(out type);
		tab.NewObj(name, constant, type); obj = tab.Find(name);
		if(obj.level==0) gen.Emit(Op.STOG, obj.adr);
		else gen.Emit(Op.STO, obj.adr); 
		while (la.kind == 9) {
			Get();
			ConstDecl();
		}
		Expect(10);
	}

	void Ident(out string name) {
		Expect(1);
		name = t.val; 
	}

	void Expr(out int type) {
		int type1; Op op; 
		SimExpr(out type);
		if (StartOf(1)) {
			RelOp(out op);
			SimExpr(out type1);
			if (type != type1) SemErr("incompatible types");
			gen.Emit(op); type = boolean; 
		}
	}

	void SimExpr(out int type) {
		int type1; Op op; 
		Term(out type);
		while (la.kind == 5 || la.kind == 6) {
			AddOp(out op);
			Term(out type1);
			if (type != integer || type1 != integer) 
			 SemErr("integer type expected");
			gen.Emit(op); 
		}
	}

	void RelOp(out Op op) {
		op = Op.EQU; 
		switch (la.kind) {
		case 22: {
			Get();
			break;
		}
		case 23: {
			Get();
			op = Op.LSS; 
			break;
		}
		case 24: {
			Get();
			op = Op.GTR; 
			break;
		}
		case 25: {
			Get();
			op = Op.NEQ; 
			break;
		}
		case 26: {
			Get();
			op = Op.LSE; 
			break;
		}
		case 27: {
			Get();
			op = Op.GTE; 
			break;
		}
		default: SynErr(46); break;
		}
	}

	void Factor(out int type) {
		int n, adr; Obj obj; string name; string s; 
		type = undef; 
		switch (la.kind) {
		case 11: {
			Get();
			Expr(out type);
			Expect(12);
			break;
		}
		case 1: {
			Ident(out name);
			obj = tab.Find(name); type = obj.type; adr = obj.adr; 
			if (la.kind == 13) {
				Get();
				if(!obj.isArray) SemErr("Not an array"); 
				Expect(2);
				n = Convert.ToInt32(t.val); 
				if( n > obj.size || n < 1) SemErr("Run-time exception - array index out of bounds"); 
				else adr = adr + (n-1); 
				Expect(14);
			} else if (StartOf(2)) {
				if(obj.isArray) SemErr("Index of array missing"); 
			} else SynErr(47);
			if (obj.kind == var || obj.kind == constant) {
			if (obj.level == 0) gen.Emit(Op.LOADG, adr);
			else gen.Emit(Op.LOAD, adr);
			} else SemErr("variable or constant expected"); 
			break;
		}
		case 2: {
			Get();
			n = Convert.ToInt32(t.val); 
			gen.Emit(Op.CONST, n); type = integer; 
			break;
		}
		case 6: {
			Get();
			Factor(out type);
			if (type != integer) {
			 SemErr("integer type expected"); type = integer;
			}
			gen.Emit(Op.NEG); 
			break;
		}
		case 15: {
			Get();
			gen.Emit(Op.CONST, 1); type = boolean; 
			break;
		}
		case 16: {
			Get();
			gen.Emit(Op.CONST, 0); type = boolean; 
			break;
		}
		case 3: {
			Get();
			s = Convert.ToString(t.val); 
			gen.Emit(Op.STR, s, s.Length); type = str; 
			break;
		}
		case 4: {
			Get();
			s = Convert.ToString(t.val); 
			gen.Emit(Op.STR, s, 3); type = str; 
			break;
		}
		default: SynErr(48); break;
		}
	}

	void MulOp(out Op op) {
		op = Op.MUL; 
		if (la.kind == 17) {
			Get();
		} else if (la.kind == 18) {
			Get();
			op = Op.DIV; 
		} else SynErr(49);
	}

	void ProcDecl() {
		string name; Obj obj; int adr; 
		Expect(19);
		Ident(out name);
		obj = tab.NewObj(name, proc, undef); obj.adr = gen.pc;
		if (name == "Main") gen.progStart = gen.pc; 
		tab.OpenScope(); 
		Expect(11);
		Expect(12);
		Expect(20);
		gen.Emit(Op.ENTER, 0); adr = gen.pc - 2; 
		while (StartOf(3)) {
			if (la.kind == 7) {
				ConstDecl();
			} else if (StartOf(4)) {
				VarDecl();
			} else {
				Stat();
			}
		}
		Expect(21);
		gen.Emit(Op.LEAVE); gen.Emit(Op.RET);
		gen.Patch(adr, tab.topScope.nextAdr);
		tab.CloseScope(); 
	}

	void VarDecl() {
		string name; int type, n=0; Obj obj;
		Type(out type);
		Ident(out name);
		tab.NewObj(name, var, type); 
		if (la.kind == 13) {
			Get();
			Expect(2);
			n = Convert.ToInt32(t.val); if(type != integer) SemErr("integer type expected for array");
			if(n<1) SemErr("Array size must be a positive integer");
			tab.toArray(name, n); obj = tab.Find(name);
			if(obj.level == 0) gen.Emit(Op.STOGA, obj.adr, n);
			else gen.Emit(Op.STOA, obj.adr, n); 
			Expect(14);
		}
		while (la.kind == 9) {
			Get();
			Ident(out name);
			tab.NewObj(name, var, type); 
			if (la.kind == 13) {
				Get();
				Expect(2);
				n = Convert.ToInt32(t.val); if(type != integer) SemErr("integer type expected for array");
				if(n<1) SemErr("Array size must be a positive integer");
				tab.toArray(name, n); obj = tab.Find(name);
				if(obj.level == 0) gen.Emit(Op.STOGA, obj.adr, n);
				else gen.Emit(Op.STOA, obj.adr, n); 
				Expect(14);
			}
		}
		Expect(10);
	}

	void Stat() {
		int type; string name; Obj obj, obj1; int n;
		int adr, adr2, adr3, loopstart; 
		switch (la.kind) {
		case 1: {
			Ident(out name);
			obj = tab.Find(name); adr3 = obj.adr; 
			if (la.kind == 8 || la.kind == 13) {
				if (la.kind == 13) {
					Get();
					if(!obj.isArray) SemErr("Identifier is not an array"); 
					Expect(2);
					n = Convert.ToInt32(t.val); 
					if( n > obj.size || n < 1) SemErr("Run-time exception - array index out of bounds"); 
					else adr3 = adr3 + (n-1); 
					Expect(14);
				} else {
					if(obj.isArray) SemErr("Array missing index"); 
				}
				Expect(8);
				if (obj.kind != var) SemErr("can only assign to variables"); 
				Expr(out type);
				if (la.kind == 28) {
					Get();
					if(type != boolean) SemErr("boolean type expected");
					gen.Emit(Op.FJMP, 0); adr = gen.pc - 2; 
					Expr(out type);
					if(type != obj.type) SemErr("incompatible types");
					gen.Emit(Op.JMP, 0); adr2 = gen.pc - 2; 
					Expect(29);
					gen.Patch(adr, gen.pc) ; 
					Expr(out type);
					if(type != obj.type) SemErr("incompatible types");
					gen.Patch(adr2, gen.pc); 
				} else if (la.kind == 10) {
					if (type != obj.type) SemErr("incompatible types"); 
				} else SynErr(50);
				if (obj.level == 0) gen.Emit(Op.STOG, adr3);
				else gen.Emit(Op.STO, adr3); 
			} else if (la.kind == 11) {
				Get();
				Expect(12);
				if (obj.kind != proc) SemErr("object is not a procedure");
				gen.Emit(Op.CALL, obj.adr); 
			} else SynErr(51);
			Expect(10);
			break;
		}
		case 30: {
			Get();
			Expect(11);
			Expr(out type);
			Expect(12);
			if (type != boolean) SemErr("boolean type expected");
			gen.Emit(Op.FJMP, 0); adr = gen.pc - 2; 
			Stat();
			if (la.kind == 31) {
				Get();
				gen.Emit(Op.JMP, 0); adr2 = gen.pc - 2;
				gen.Patch(adr, gen.pc);
				adr = adr2; 
				Stat();
			}
			gen.Patch(adr, gen.pc); 
			break;
		}
		case 32: {
			Get();
			loopstart = gen.pc; 
			Expect(11);
			Expr(out type);
			Expect(12);
			if (type != boolean) SemErr("boolean type expected");
			gen.Emit(Op.FJMP, 0); adr = gen.pc - 2; 
			Stat();
			gen.Emit(Op.JMP, loopstart); gen.Patch(adr, gen.pc); 
			break;
		}
		case 33: {
			Get();
			loopstart = gen.pc; 
			Stat();
			Expect(32);
			Expect(11);
			Expr(out type);
			Expect(34);
			if (type != boolean) SemErr("boolean type expected");
			gen.Emit(Op.FJMP, 0); adr = gen.pc - 2; 
			gen.Emit(Op.JMP, loopstart); gen.Patch(adr, gen.pc); 
			break;
		}
		case 35: {
			Get();
			Ident(out name);
			Expect(10);
			obj = tab.Find(name);
			if (obj.type != integer) SemErr("integer type expected");
			gen.Emit(Op.READ);
			if (obj.level == 0) gen.Emit(Op.STOG, obj.adr);
			else gen.Emit(Op.STO, obj.adr); 
			break;
		}
		case 36: {
			Get();
			Expr(out type);
			if (type != integer && type != str) SemErr("integer or string type expected");
			if(type == integer)  gen.Emit(Op.WRITE); 
			else gen.Emit(Op.WRITES); 
			while (la.kind == 9) {
				Get();
				gen.Emit(Op.WRITEC); 
				Expr(out type);
				if (type != integer && type != str) SemErr("integer or string type expected");
				if(type == integer) gen.Emit(Op.WRITE); 
				else gen.Emit(Op.WRITES); 
			}
			Expect(10);
			gen.Emit(Op.WRITEL); 
			break;
		}
		case 20: {
			Get();
			while (StartOf(5)) {
				if (StartOf(6)) {
					Stat();
				} else {
					VarDecl();
				}
			}
			Expect(21);
			break;
		}
		case 37: {
			Get();
			Ident(out name);
			obj = tab.Find(name); if(obj.kind != var && obj.type != integer) SemErr("integer variable needed in for loop"); 
			Expect(8);
			if (la.kind == 2) {
				Get();
				n = Convert.ToInt32(t.val);
				gen.Emit(Op.CONST, n); 
			} else if (la.kind == 1) {
				Ident(out name);
				obj1 = tab.Find(name); if(obj1.kind != constant && obj1.type != integer) SemErr("integer constant expected"); 
				if(obj1.level==0) gen.Emit(Op.LOADG, obj1.adr);
				else gen.Emit(Op.LOAD, obj1.adr);
				
			} else SynErr(52);
			if(obj.level==0) gen.Emit(Op.STOG, obj.adr);
			else gen.Emit(Op.STO, obj.adr); 
			loopstart = gen.pc; 
			if (obj.level == 0) gen.Emit(Op.LOADG, obj.adr);
			else gen.Emit(Op.LOAD, obj.adr); 
			Expect(38);
			if (la.kind == 2) {
				Get();
				n = Convert.ToInt32(t.val); type = integer;
				gen.Emit(Op.CONST, n); 
			} else if (la.kind == 1) {
				Ident(out name);
				obj1 = tab.Find(name); if(obj1.kind != constant && obj1.type != integer) SemErr("integer constant expected"); 
				if(obj1.level==0) gen.Emit(Op.LOADG, obj1.adr);
				else gen.Emit(Op.LOAD, obj1.adr); 
			} else SynErr(53);
			gen.Emit(Op.LSE); gen.Emit(Op.FJMP, 0); adr = gen.pc - 2; 
			Expect(33);
			Stat();
			if(obj.level==0) gen.Emit(Op.LOAD, obj.adr); 
			else gen.Emit(Op.LOADG, obj.adr); 
			gen.Emit(Op.CONST, 1); gen.Emit(Op.ADD); 
			if(obj.level==0) gen.Emit(Op.STOG, obj.adr);
			else gen.Emit(Op.STO, obj.adr); 
			gen.Emit(Op.JMP, loopstart); gen.Patch(adr, gen.pc); 
			break;
		}
		default: SynErr(54); break;
		}
	}

	void Term(out int type) {
		int type1; Op op; 
		Factor(out type);
		while (la.kind == 17 || la.kind == 18) {
			MulOp(out op);
			Factor(out type1);
			if (type != integer || type1 != integer) 
			 SemErr("integer type expected");
			gen.Emit(op); 
		}
	}

	void Taste() {
		string name; 
		Expect(39);
		Ident(out name);
		tab.OpenScope(); 
		Expect(20);
		while (StartOf(7)) {
			if (la.kind == 7) {
				ConstDecl();
			} else if (StartOf(4)) {
				VarDecl();
			} else {
				ProcDecl();
			}
		}
		Expect(21);
		tab.CloseScope();
		if (gen.progStart == -1) SemErr("main function never defined");
		
	}

	void Type(out int type) {
		type = undef; 
		if (la.kind == 40) {
			Get();
			type = integer; 
		} else if (la.kind == 41) {
			Get();
			type = boolean; 
		} else if (la.kind == 42) {
			Get();
			type = str; 
		} else if (la.kind == 43) {
			Get();
			type = str; 
		} else SynErr(55);
	}



	public void Parse() {
		la = new Token();
		la.val = "";		
		Get();
		Taste();
		Expect(0);

	}
	
	static readonly bool[,] set = {
		{T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, T,T,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,T,T,x, x,T,T,x, T,x,x,x, x,T,T,x, x,x,T,T, T,T,T,T, T,T,x,x, x,x,T,x, x,x,x,x, x,x,x,x, x,x},
		{x,T,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,T,x, T,T,x,T, T,T,x,x, T,T,T,T, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,T,T, x,x},
		{x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,T,x, T,T,x,T, T,T,x,x, T,T,T,T, x,x},
		{x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,T,x, T,T,x,T, T,T,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,T,T, x,x}

	};
} // end Parser


public class Errors {
	public int count = 0;                                    // number of errors detected
	public System.IO.TextWriter errorStream = Console.Out;   // error messages go to this stream
	public string errMsgFormat = "-- line {0} col {1}: {2}"; // 0=line, 1=column, 2=text

	public virtual void SynErr (int line, int col, int n) {
		string s;
		switch (n) {
			case 0: s = "EOF expected"; break;
			case 1: s = "ident expected"; break;
			case 2: s = "number expected"; break;
			case 3: s = "strT expected"; break;
			case 4: s = "charT expected"; break;
			case 5: s = "\"+\" expected"; break;
			case 6: s = "\"-\" expected"; break;
			case 7: s = "\"constant\" expected"; break;
			case 8: s = "\":=\" expected"; break;
			case 9: s = "\",\" expected"; break;
			case 10: s = "\";\" expected"; break;
			case 11: s = "\"(\" expected"; break;
			case 12: s = "\")\" expected"; break;
			case 13: s = "\"[\" expected"; break;
			case 14: s = "\"]\" expected"; break;
			case 15: s = "\"true\" expected"; break;
			case 16: s = "\"false\" expected"; break;
			case 17: s = "\"*\" expected"; break;
			case 18: s = "\"/\" expected"; break;
			case 19: s = "\"void\" expected"; break;
			case 20: s = "\"{\" expected"; break;
			case 21: s = "\"}\" expected"; break;
			case 22: s = "\"==\" expected"; break;
			case 23: s = "\"<\" expected"; break;
			case 24: s = "\">\" expected"; break;
			case 25: s = "\"!=\" expected"; break;
			case 26: s = "\"<=\" expected"; break;
			case 27: s = "\">=\" expected"; break;
			case 28: s = "\"?\" expected"; break;
			case 29: s = "\":\" expected"; break;
			case 30: s = "\"if\" expected"; break;
			case 31: s = "\"else\" expected"; break;
			case 32: s = "\"while\" expected"; break;
			case 33: s = "\"do\" expected"; break;
			case 34: s = "\");\" expected"; break;
			case 35: s = "\"read\" expected"; break;
			case 36: s = "\"write\" expected"; break;
			case 37: s = "\"for\" expected"; break;
			case 38: s = "\"to\" expected"; break;
			case 39: s = "\"program\" expected"; break;
			case 40: s = "\"int\" expected"; break;
			case 41: s = "\"bool\" expected"; break;
			case 42: s = "\"string\" expected"; break;
			case 43: s = "\"char\" expected"; break;
			case 44: s = "??? expected"; break;
			case 45: s = "invalid AddOp"; break;
			case 46: s = "invalid RelOp"; break;
			case 47: s = "invalid Factor"; break;
			case 48: s = "invalid Factor"; break;
			case 49: s = "invalid MulOp"; break;
			case 50: s = "invalid Stat"; break;
			case 51: s = "invalid Stat"; break;
			case 52: s = "invalid Stat"; break;
			case 53: s = "invalid Stat"; break;
			case 54: s = "invalid Stat"; break;
			case 55: s = "invalid Type"; break;

			default: s = "error " + n; break;
		}
		errorStream.WriteLine(errMsgFormat, line, col, s);
		count++;
	}

	public virtual void SemErr (int line, int col, string s) {
		errorStream.WriteLine(errMsgFormat, line, col, s);
		count++;
	}
	
	public virtual void SemErr (string s) {
		errorStream.WriteLine(s);
		count++;
	}
	
	public virtual void Warning (int line, int col, string s) {
		errorStream.WriteLine(errMsgFormat, line, col, s);
	}
	
	public virtual void Warning(string s) {
		errorStream.WriteLine(s);
	}
} // Errors


public class FatalError: Exception {
	public FatalError(string m): base(m) {}
}
}