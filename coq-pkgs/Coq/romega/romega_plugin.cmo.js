(function(iY){"use strict";var
ch="xO",cm=" - ",cn="not",cC=140,j=250,cl="Z.succ",cA="not a number",cB="Z.pred",bs="Z.opp",cq="xH",aZ="Zpos",G=124,cz="REIFED PROBLEM\n\n",e=246,ck="FF",bp="Z.sub",bt=119,cI="romega_plugin",ct="True",au="Z",cy="Extension: cannot occur",aa="romega",cx="\n====================================\n",br="N",cF="False",cs="with",bq="Z.mul",cE="Prop",cw="romega'",aY="",cp="or",cv="TT",a0="Omega",cg="=SYSTEM===================================\n",aX="Zneg",S=136,bo="Z.add",cj="positive",aW="Z0",cu="  Depends on:",cD="nat",cH="  CONCL: ",cr="and",cG="Omega: Can't solve a goal with non-linear products",cf="C_MULT_OPP_LEFT",ci="\n------------------------------------\n",co="\n",ce="xI",$="Coq",y=iY.jsoo_runtime,_=y.caml_equal,b=y.caml_new_string,i=y.caml_obj_tag,aV=y.caml_register_global,n=y.caml_string_notequal,cd=y.caml_trampoline,cc=y.caml_trampoline_return,u=y.caml_wrap_exception;function
a(a,b){return a.length==1?a(b):y.caml_call_gen(a,[b])}function
d(a,b,c){return a.length==2?a(b,c):y.caml_call_gen(a,[b,c])}function
t(a,b,c,d){return a.length==3?a(b,c,d):y.caml_call_gen(a,[b,c,d])}function
bn(a,b,c,d,e){return a.length==4?a(b,c,d,e):y.caml_call_gen(a,[b,c,d,e])}function
iX(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):y.caml_call_gen(a,[b,c,d,e,f])}function
ah(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):y.caml_call_gen(a,[b,c,d,e,f,g])}function
cb(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):y.caml_call_gen(a,[b,c,d,e,f,g,h])}var
p=y.caml_get_global_data(),Z=b(cI),g=p.CamlinternalLazy,s=p.Term,r=p.Bigint,J=p.CErrors,a2=p.Tacmach,av=p.Logic,v=p.Names,k=p.Pervasives,a1=p.Univ,I=p.Coqlib,bu=p.Global,a3=p.Nametab,f=p.Util,l=p.Printf,E=p.Hashtbl,H=p.Not_found,ab=p.Tactics,U=p.Proofview,bH=p.Invalid_argument,w=p.Pp,ac=p.Tacticals,b$=p.Constrarg,b9=p.Loc,bk=p.List,b_=p.Genarg,b8=p.Tacinterp,aT=p.Tacenv,bj=p.Mltop,bv=[0,b($),[0,b(aa),[0,b("ReflOmegaCore"),0]]],gJ=b(bo),gK=b(bq),gL=b(bs),gM=b(cB),gN=b(bp),gO=b(cl),gP=b(aW),gQ=b(aX),gR=b(aZ),gC=b("Z.ge"),gD=b("Z.gt"),gE=b("Z.le"),gF=b("Z.lt"),gG=b("Zne"),gH=b("eq"),gI=[1,b(au),0],gs=b(bo),gt=b(bq),gu=b(bs),gv=b(cB),gw=b(bp),gx=b(cl),gy=b(aW),gz=b(aX),gA=b(aZ),gj=b(cq),gk=b(ce),gl=b(ch),gm=b(cA),gn=b(aW),go=b(aX),gp=b(aZ),gq=b(cA),gi=b(aX),gh=b(aZ),gg=b(aW),gf=b(ce),ge=b(ch),gd=b(cq),gb=b(bp),f$=b(bs),f9=b(bq),f7=b(bo),f5=b(au),fZ=b(cF),f0=b(ct),f1=b(cr),f2=b("iff"),f3=b(cn),f4=b(cp),fT=b("nil"),fS=b("cons"),fQ=[0,b("Init"),[0,b("Datatypes"),0]],fR=b(aY),fP=b("O"),fO=b("S"),fL=b("do_omega"),fJ=b("interp_goal_concl"),fH=b("E_SOLVE"),fF=b("E_EXTRACT"),fD=b("E_SPLIT"),fB=b("D_mono"),fz=b("D_right"),fx=b("D_left"),fv=b("direction"),ft=b("O_NEGATE_CONTRADICT_INV"),fr=b("O_NEGATE_CONTRADICT"),fp=b("O_CONSTANT_NUL"),fn=b("O_SPLIT_INEQ"),fl=b("O_MERGE_EQ"),fj=b("O_CONTRADICTION"),fh=b("O_STATE"),ff=b("O_SUM"),fd=b("O_EXACT_DIVIDE"),fb=b("O_NOT_EXACT_DIVIDE"),e$=b("O_DIV_APPROX"),e9=b("O_CONSTANT_NEG"),e7=b("O_CONSTANT_NOT_NUL"),e5=b("C_MULT_COMM"),e3=b("C_MINUS"),e1=b("C_MULT_ASSOC_REDUCED"),eZ=b(cf),eX=b("C_RED6"),eV=b("C_RED5"),eT=b("C_RED4"),eR=b("C_RED3"),eP=b("C_RED2"),eN=b("C_RED1"),eL=b("C_RED0"),eJ=b("C_PLUS_COMM"),eH=b("C_PLUS_PERMUTE"),eF=b("C_PLUS_ASSOC_L"),eD=b("C_PLUS_ASSOC_R"),eB=b("C_MULT_ASSOC_R"),ez=b(cf),ex=b("C_MULT_PLUS_DISTR"),ev=b("C_REDUCE"),et=b("C_OPP_ONE"),er=b("C_OPP_MULT_R"),ep=b("C_OPP_OPP"),en=b("C_OPP_PLUS"),em=b("C_NOP"),el=b("C_SEQ"),ek=b("C_RIGHT"),ej=b("C_LEFT"),ei=b("C_DO_BOTH"),eg=b("F_right"),ee=b("F_left"),ec=b("F_cancel"),ea=b("F_equal"),d$=b("t_fusion"),d9=b("Tprop"),d7=b("Timp"),d5=b("Tand"),d3=b("Tor"),d1=b("Tnot"),dZ=b("FalseTerm"),dX=b("TrueTerm"),dV=b("NeqTerm"),dT=b("GtTerm"),dR=b("LtTerm"),dP=b("GeqTerm"),dN=b("LeqTerm"),dL=b("EqTerm"),dJ=b("proposition"),dH=b("Tvar"),dF=b("Tminus"),dD=b("Topp"),dB=b("Tmult"),dz=b("Tplus"),dx=b("Tint"),dv=b("P_STEP"),dt=b("P_INVERT"),dr=b("P_RIGHT"),dp=b("P_LEFT"),dm=b("pair_step"),dk=b("h_step"),di=b("I"),dg=b(cF),de=b(ct),dc=b(cp),da=b(cn),c_=b(cr),c8=b("eq_refl"),cP=b("Omega: Not a quantifier-free goal"),cM=b("."),cN=b(aY),cO=b(aY),cL=[0,b(au),[0,b(br),[0,b("Pos"),0]]],cQ=b("Const_omega.Destruct"),cS=[0,b($),[0,b("Logic"),[0,b("Decidable"),0]]],cT=[0,b("ZOmega"),0],cW=[0,[0,b($),[0,b("Lists"),[0,b("List"),0]]],0],c2=[0,[0,b($),[0,b("Numbers"),[0,b("BinNums"),0]]],0],c3=[0,[0,b($),[0,b("ZArith"),[0,b("BinInt"),0]]],0],c4=b(a0),c5=b(a0),c6=b(a0),c7=b(a0),g_=[0,[2,0,0],b("%s")],g$=[0,[12,40,[15,[11,b(" + "),[15,[12,41,0]]]]],b("(%a + %a)")],ha=[0,[12,40,[15,[11,b(" * "),[15,[12,41,0]]]]],b("(%a * %a)")],hb=[0,[12,40,[15,[11,b(cm),[15,[12,41,0]]]]],b("(%a - %a)")],hc=[0,[11,b("~ "),[15,0]],b("~ %a")],hd=[0,[12,86,[4,0,[0,2,2],0,0]],b("V%02d")],he=[0,[12,63,0],b("?")],hf=[0,[11,b(cv),0],b(cv)],hg=[0,[11,b(ck),0],b(ck)],hh=b("="),hj=b("<="),hk=b(">="),hl=b(">"),hm=b("<"),hn=b("!="),hi=[0,[15,[12,32,[2,0,[12,32,[15,0]]]]],b("%a %s %a")],ho=[0,[11,b("not("),[15,[12,41,0]]],b("not(%a)")],hp=[0,[12,40,[15,[11,b(" or "),[15,[12,41,0]]]]],b("(%a or %a)")],hq=[0,[12,40,[15,[11,b(" and "),[15,[12,41,0]]]]],b("(%a and %a)")],hr=[0,[12,40,[15,[11,b(" => "),[15,[12,41,0]]]]],b("(%a => %a)")],hs=[0,[11,b(cE),0],b(cE)],ht=b("weight"),hu=b("weight minus"),hA=b(cG),hB=b("scalar minus"),hC=b(cG),hD=b("negate minus"),h3=b(" "),h4=[0,[4,0,0,0,[12,91,[2,0,[12,93,0]]]],b("%d[%s]")],h5=[0,[12,83,[4,0,0,0,[12,40,[15,[12,44,[15,[12,41,0]]]]]]],b("S%d(%a,%a)")],h6=[0,[4,0,0,0,[11,b(cm),[4,0,0,0,[12,10,0]]]],b("%d - %d\n")],h$=[0,[11,b("Cannot find constructor "),[4,0,0,0,0]],b("Cannot find constructor %d")],ia=[0,0,0],ib=[0,1,0],ic=[0,[11,b("Cannot find equation "),[4,0,0,0,0]],b("Cannot find equation %d")],id=[0,2,0],il=[0,b($),[0,b(aa),[0,b("ROmega"),0]]],im=b("ROmega can't solve this system"),ih=[0,[12,32,[4,0,0,0,0]],b(" %d")],ie=[0,[11,b("SYSTEME "),[4,0,0,0,[12,10,0]]],b("SYSTEME %d\n")],ig=b("\n  Depend :"),ii=b("\n  Split points :"),ij=[0,[11,b(ci),0],b(ci)],ik=[0,[11,b(cx),0],b(cx)],h9=b("not_treated"),h_=b("no contradiction"),h8=[0,[11,b("get_hyp "),[4,0,0,0,0]],b("get_hyp %d")],h7=b("find_path"),h2=b("select_smaller"),h1=[0,[11,b(cg),0],b(cg)],hT=b("L"),hU=b("R"),hV=b("M"),hS=[0,[11,b(cu),0],b(cu)],hW=b(aY),hX=[0,[11,b("\n  Path: "),[2,0,0]],b("\n  Path: %s")],hY=b("yes"),h0=b("no"),hZ=[0,[11,b("\n  Origin: "),[2,0,[11,b(" (negated : "),[2,0,[11,b(")\n\n"),0]]]]],b("\n  Origin: %s (negated : %s)\n\n")],hR=[0,[11,b("  E"),[4,0,0,0,[11,b(" : "),[15,[12,32,[2,0,[11,b(" 0\n"),0]]]]]]],b("  E%d : %a %s 0\n")],hP=[0,[11,b(" L"),[4,0,0,0,0]],b(" L%d")],hQ=[0,[11,b(" R"),[4,0,0,0,0]],b(" R%d")],hO=[0,0,0],hM=[0,[11,b("  "),[2,0,[11,b(": "),0]]],b("  %s: ")],hN=[0,[12,10,0],b(co)],hI=[0,2,0],hJ=[0,[11,b(cz),0],b(cz)],hK=[0,[11,b(cH),0],b(cH)],hL=[0,[12,10,0],b(co)],hH=b("condense.1"),hG=b("reduce_factor.1"),hF=b("shrink.1"),hy=[0,[4,0,0,0,[11,b(" -> "),[4,0,0,0,[12,10,0]]]],b("%d -> %d\n")],hx=[0,[11,b("Atome "),[4,0,0,0,[11,b(" non trouv\xc3\xa9\n"),0]]],b("Atome %d non trouv\xc3\xa9\n")],hv=b("CO"),hw=b("compile_equation"),g9=[0,[11,b("Omega Equation "),[4,0,0,0,[11,b(" non trouv\xc3\xa9e\n"),0]]],b("Omega Equation %d non trouv\xc3\xa9e\n")],g8=b("get_prop"),g6=b("get_reified_atom"),g5=b("unintern"),g4=[0,[11,b("OV"),[4,0,0,0,0]],b("OV%d")],gZ=[0,[12,40,[0,[4,0,[0,2,2],0,[12,41,0]]]],b("(%c%02d)")],g0=b(" := "),g1=b("  ===============================\n\n"),g2=b("ENVIRONMENT OF PROPOSITIONS :"),g3=b("ENVIRONMENT OF TERMS :"),gX=b("__goal__"),iT=[0,b("plugins/romega/g_romega.ml4"),1,0],iR=[0,[0,[0,b(aa)],[0,[0,b(cs)],[0,[0,b("*")],0]]],0],iS=b("$l"),iU=[0,b(cs)],iV=[0,b(aa)],iW=b(cw),iM=b(cy),iK=[0,b(cD),[0,b(cj),[0,b(br),[0,b(au),0]]]],iJ=b(cy),iE=b(aa),iF=b(aa),iu=b(br),iv=b(au),iw=b(cD),ix=b(cj),iz=b("zify_positive"),iA=b("zify_nat"),iB=b("zify_op"),iC=b("zify_N"),iy=b("No ROmega knowledge base for type "),it=[0,b("PreOmega"),[0,b("omega"),[0,b($),0]]],is=b(cI),iH=b(aa),iP=b(cw),cK=p.Globnames,cJ=p.Universes,gS=p.Failure,gT=p.Printer,gV=p.Feedback,gU=p.Omega_plugin,io=p.Assert_failure,ip=p.Tacentries,iq=p.String,ir=p.Array;function
a4(b){var
h=a(a3[41],b),c=a(v[5][5],h);if(c)var
e=a(v[1][7],c[1]),i=d(f[15][45][2],e,cL)?d(k[16],e,cM):cN,g=i;else
var
g=cO;var
j=a(a3[42],b),l=a(v[1][7],j);return d(k[16],g,l)}function
ai(e){var
d=a(s[39],e),c=d[2],b=a(s[cC],d[1]);switch(b[0]){case
1:if(!c)return[0,a(v[1][7],b[1])];break;case
6:if(b[1]){if(!c)return a(J[6],cP)}else
if(!c)return[2,b[2],b[3]];break;case
10:return[1,a4([1,b[1][1]]),c];case
11:return[1,a4([2,b[1][1]]),c];case
12:return[1,a4([3,b[1][1]]),c]}return 0}var
cR=[248,cQ,y.caml_fresh_oo_id(0)];function
bw(e){var
d=a(s[39],e),f=d[2],b=a(s[cC],d[1]);switch(b[0]){case
10:var
c=[1,b[1][1]];break;case
11:var
c=[2,b[1][1]];break;case
12:var
c=[3,b[1][1]];break;default:throw cR}return[0,a(a3[42],c),f]}var
cU=[0,d(k[22],bv,cT),0],cV=d(k[22],[0,bv,0],cU),cX=d(k[22],cW,cV),cY=d(k[22],I[9],cX),cZ=d(k[22],I[8],cY),c0=d(k[22],[0,cS,0],cZ),c1=d(k[22],I[10],c0),K=d(I[6],c4,I[10]),h=d(I[6],c5,c1),aw=d(I[6],c6,c3),T=d(I[6],c7,c2),c9=[e,function(b){return a(K,c8)}],c$=[e,function(b){return a(K,c_)}],db=[e,function(b){return a(K,da)}],dd=[e,function(b){return a(K,dc)}],df=[e,function(b){return a(K,de)}],dh=[e,function(b){return a(K,dg)}],dj=[e,function(b){return a(K,di)}],dl=[e,function(b){return a(h,dk)}],dn=[e,function(b){return a(h,dm)}],dq=[e,function(b){return a(h,dp)}],ds=[e,function(b){return a(h,dr)}],du=[e,function(b){return a(h,dt)}],dw=[e,function(b){return a(h,dv)}],dy=[e,function(b){return a(h,dx)}],dA=[e,function(b){return a(h,dz)}],dC=[e,function(b){return a(h,dB)}],dE=[e,function(b){return a(h,dD)}],dG=[e,function(b){return a(h,dF)}],dI=[e,function(b){return a(h,dH)}],dK=[e,function(b){return a(h,dJ)}],dM=[e,function(b){return a(h,dL)}],dO=[e,function(b){return a(h,dN)}],dQ=[e,function(b){return a(h,dP)}],dS=[e,function(b){return a(h,dR)}],dU=[e,function(b){return a(h,dT)}],dW=[e,function(b){return a(h,dV)}],dY=[e,function(b){return a(h,dX)}],d0=[e,function(b){return a(h,dZ)}],d2=[e,function(b){return a(h,d1)}],d4=[e,function(b){return a(h,d3)}],d6=[e,function(b){return a(h,d5)}],d8=[e,function(b){return a(h,d7)}],d_=[e,function(b){return a(h,d9)}],ax=[e,function(b){return a(h,d$)}],eb=[e,function(b){return a(h,ea)}],ed=[e,function(b){return a(h,ec)}],ef=[e,function(b){return a(h,ee)}],eh=[e,function(b){return a(h,eg)}],aj=[e,function(b){return a(h,ei)}],ak=[e,function(b){return a(h,ej)}],al=[e,function(b){return a(h,ek)}],am=[e,function(b){return a(h,el)}],q=[e,function(b){return a(h,em)}],eo=[e,function(b){return a(h,en)}],eq=[e,function(b){return a(h,ep)}],es=[e,function(b){return a(h,er)}],eu=[e,function(b){return a(h,et)}],ew=[e,function(b){return a(h,ev)}],ey=[e,function(b){return a(h,ex)}],eA=[e,function(b){return a(h,ez)}],eC=[e,function(b){return a(h,eB)}],eE=[e,function(b){return a(h,eD)}],eG=[e,function(b){return a(h,eF)}],eI=[e,function(b){return a(h,eH)}],eK=[e,function(b){return a(h,eJ)}],eM=[e,function(b){return a(h,eL)}],eO=[e,function(b){return a(h,eN)}],eQ=[e,function(b){return a(h,eP)}],eS=[e,function(b){return a(h,eR)}],eU=[e,function(b){return a(h,eT)}],eW=[e,function(b){return a(h,eV)}],eY=[e,function(b){return a(h,eX)}],e0=[e,function(b){return a(h,eZ)}],e2=[e,function(b){return a(h,e1)}],e4=[e,function(b){return a(h,e3)}],e6=[e,function(b){return a(h,e5)}],e8=[e,function(b){return a(h,e7)}],e_=[e,function(b){return a(h,e9)}],fa=[e,function(b){return a(h,e$)}],fc=[e,function(b){return a(h,fb)}],fe=[e,function(b){return a(h,fd)}],fg=[e,function(b){return a(h,ff)}],fi=[e,function(b){return a(h,fh)}],fk=[e,function(b){return a(h,fj)}],fm=[e,function(b){return a(h,fl)}],fo=[e,function(b){return a(h,fn)}],fq=[e,function(b){return a(h,fp)}],fs=[e,function(b){return a(h,fr)}],fu=[e,function(b){return a(h,ft)}],fw=[e,function(b){return a(h,fv)}],fy=[e,function(b){return a(h,fx)}],fA=[e,function(b){return a(h,fz)}],fC=[e,function(b){return a(h,fB)}],fE=[e,function(b){return a(h,fD)}],fG=[e,function(b){return a(h,fF)}],fI=[e,function(b){return a(h,fH)}],fK=[e,function(b){return a(h,fJ)}],fM=[e,function(b){return a(h,fL)}];function
bx(b){var
c=i(q),k=j===c?q[1]:e===c?a(g[2],q):q;if(d(s[S],b,k)){var
f=i(q);return j===f?q[1]:e===f?a(g[2],q):q}var
h=i(ak),l=[0,b],m=j===h?ak[1]:e===h?a(g[2],ak):ak;return a(s[G],[0,m,l])}function
by(b){var
c=i(q),k=j===c?q[1]:e===c?a(g[2],q):q;if(d(s[S],b,k)){var
f=i(q);return j===f?q[1]:e===f?a(g[2],q):q}var
h=i(al),l=[0,b],m=j===h?al[1]:e===h?a(g[2],al):al;return a(s[G],[0,m,l])}function
fN(c,b){var
f=i(q),l=j===f?q[1]:e===f?a(g[2],q):q;if(d(s[S],c,l))return by(b);var
h=i(q),m=j===h?q[1]:e===h?a(g[2],q):q;if(d(s[S],b,m))return bx(c);var
k=i(aj),n=[0,c,b],o=j===k?aj[1]:e===k?a(g[2],aj):aj;return a(s[G],[0,o,n])}function
bz(c,b){var
f=i(q),l=j===f?q[1]:e===f?a(g[2],q):q;if(d(s[S],c,l))return b;var
h=i(q),m=j===h?q[1]:e===h?a(g[2],q):q;if(d(s[S],b,m))return c;var
k=i(am),n=[0,c,b],o=j===k?am[1]:e===k?a(g[2],am):am;return a(s[G],[0,o,n])}function
bA(b){if(b){var
c=b[2],d=b[1];return c?bz(d,bA(c)):d}var
f=i(q);return j===f?q[1]:e===f?a(g[2],q):q}var
ay=[e,function(b){return a(K,fO)}],az=[e,function(b){return a(K,fP)}];function
bB(b){if(0===b){var
c=i(az);return j===c?az[1]:e===c?a(g[2],az):az}var
d=i(ay),f=[0,bB(b-1|0)],h=j===d?ay[1]:e===d?a(g[2],ay):ay;return a(s[G],[0,h,f])}function
bC(c){var
b=t(I[5],fR,fQ,c),d=a(bu[45],b)?function(b){return a(a1[29][3],[0,b])}:function(a){return a1[29][1]};return function(c){var
e=d(c),f=[0,a(cK[10],b),e];return a(s[131],f)}}function
bD(d,c,b){function
e(b){if(b){var
h=b[1],i=[0,h,e(b[2])],f=[0,a(bC(fS),d),[0,c]],j=[0,a(s[G],f),i];return a(s[G],j)}var
g=[0,a(bC(fT),d),[0,c]];return a(s[G],g)}return e(b)}var
fU=a(bu[54],0),fV=a(cJ[9],fU);function
fW(a){return bD(fV,s[117],a)}var
fX=a1[1][1];function
bE(a,b){return bD(fX,a,b)}function
fY(c){var
b=i(ax),d=j===b?ax[1]:e===b?a(g[2],ax):ax;return bE(d,c)}var
f6=[e,function(b){return a(T,f5)}],f8=[e,function(b){return a(aw,f7)}],f_=[e,function(b){return a(aw,f9)}],ga=[e,function(b){return a(aw,f$)}],gc=[e,function(b){return a(aw,gb)}],aA=[e,function(b){return a(T,gd)}],aB=[e,function(b){return a(T,ge)}],aC=[e,function(b){return a(T,gf)}],aD=[e,function(b){return a(T,gg)}],aE=[e,function(b){return a(T,gh)}],aF=[e,function(b){return a(T,gi)}];function
bF(g){function
c(g){var
f=bw(g),b=f[2],e=a(v[1][7],f[1]);if(n(e,gj)){if(n(e,gk)){if(!n(e,gl))if(b)if(!b[2]){var
h=c(b[1]);return d(r[14],r[7],h)}}else
if(b)if(!b[2]){var
i=c(b[1]),j=d(r[14],r[7],i);return d(r[12],r[6],j)}}else
if(!b)return r[6];return a(k[2],gm)}var
f=bw(g),b=f[2],e=a(v[1][7],f[1]);if(n(e,gn)){if(n(e,go)){if(!n(e,gp))if(b)if(!b[2])return c(b[1])}else
if(b)if(!b[2]){var
h=c(b[1]);return a(r[22],h)}}else
if(!b)return r[5];return a(k[2],gq)}function
a5(b){if(_(b,r[6])){var
c=i(aA);return j===c?aA[1]:e===c?a(g[2],aA):aA}var
f=d(r[15],b,r[7]),m=f[2],n=[0,a5(f[1])];if(_(m,r[5]))var
h=i(aB),o=j===h?aB[1]:e===h?a(g[2],aB):aB,k=o;else
var
l=i(aC),p=j===l?aC[1]:e===l?a(g[2],aC):aC,k=p;return a(s[G],[0,k,n])}function
bG(b){if(_(b,r[5])){var
c=i(aD);return j===c?aD[1]:e===c?a(g[2],aD):aD}if(a(r[18],b)){var
d=i(aE),h=[0,a5(b)],k=j===d?aE[1]:e===d?a(g[2],aE):aE;return a(s[G],[0,k,h])}var
f=i(aF),l=[0,a5(a(r[22],b))],m=j===f?aF[1]:e===f?a(g[2],aF):aF;return a(s[G],[0,m,l])}function
gr(s){try{var
d=ai(s);if(typeof
d==="number")var
c=0;else
if(1===d[0]){var
e=d[1];if(n(e,gs))if(n(e,gt))if(n(e,gu))if(n(e,gv))if(n(e,gw))if(n(e,gx)){if(n(e,gy))if(n(e,gz))if(n(e,gA))var
c=0,b=0,g=0;else
var
g=1;else
var
g=1;else
var
g=1;if(g)try{var
v=[5,bF(s)],f=v,b=1}catch(c){c=u(c);if(!a(J[22],c))throw c;var
f=0,b=1}}else{var
h=d[2];if(h)if(h[2])var
c=0,b=0;else
var
f=[4,h[1]],b=1;else
var
c=0,b=0}else{var
i=d[2];if(i){var
j=i[2];if(j)if(j[2])var
c=0,b=0;else
var
f=[2,i[1],j[1]],b=1;else
var
c=0,b=0}else
var
c=0,b=0}else{var
k=d[2];if(k)if(k[2])var
c=0,b=0;else
var
w=k[1],f=[0,w,bG(a(r[22],r[6]))],b=1;else
var
c=0,b=0}else{var
l=d[2];if(l)if(l[2])var
c=0,b=0;else
var
f=[3,l[1]],b=1;else
var
c=0,b=0}else{var
m=d[2];if(m){var
o=m[2];if(o)if(o[2])var
c=0,b=0;else
var
f=[1,m[1],o[1]],b=1;else
var
c=0,b=0}else
var
c=0,b=0}else{var
p=d[2];if(p){var
q=p[2];if(q)if(q[2])var
c=0,b=0;else
var
f=[0,p[1],q[1]],b=1;else
var
c=0,b=0}else
var
c=0,b=0}if(b)var
t=f,c=1}else
var
c=0;if(!c)var
t=0;return t}catch(b){b=u(b);if(a(av[4],b))return 0;throw b}}function
gB(M,J){try{var
h=ai(J);if(typeof
h==="number")var
c=0;else
if(1===h[0]){var
k=h[1];if(n(k,gC))if(n(k,gD))if(n(k,gE))if(n(k,gF))if(n(k,gG))if(n(k,gH))var
c=0,b=0;else{var
w=h[2];if(w){var
x=w[2];if(x){var
y=x[2];if(y)if(y[2])var
c=0,b=0;else{var
N=y[1],O=x[1];if(_(ai(d(a2[29],M,w[1])),gI))var
l=[0,O,N],b=1;else
var
c=0,b=0}else
var
c=0,b=0}else
var
c=0,b=0}else
var
c=0,b=0}else{var
z=h[2];if(z){var
A=z[2];if(A)if(A[2])var
c=0,b=0;else
var
l=[1,z[1],A[1]],b=1;else
var
c=0,b=0}else
var
c=0,b=0}else{var
B=h[2];if(B){var
C=B[2];if(C)if(C[2])var
c=0,b=0;else
var
l=[2,B[1],C[1]],b=1;else
var
c=0,b=0}else
var
c=0,b=0}else{var
D=h[2];if(D){var
E=D[2];if(E)if(E[2])var
c=0,b=0;else
var
l=[3,D[1],E[1]],b=1;else
var
c=0,b=0}else
var
c=0,b=0}else{var
F=h[2];if(F){var
G=F[2];if(G)if(G[2])var
c=0,b=0;else
var
l=[4,F[1],G[1]],b=1;else
var
c=0,b=0}else
var
c=0,b=0}else{var
H=h[2];if(H){var
I=H[2];if(I)if(I[2])var
c=0,b=0;else
var
l=[5,H[1],I[1]],b=1;else
var
c=0,b=0}else
var
c=0,b=0}if(b)var
L=l,c=1}else
var
c=0;if(!c){try{var
g=ai(J);if(typeof
g==="number")var
f=0;else
switch(g[0]){case
1:var
i=g[1];if(n(i,fZ))if(n(i,f0))if(n(i,f1))if(n(i,f2))if(n(i,f3))if(n(i,f4))var
f=0,e=0;else{var
o=g[2];if(o){var
p=o[2];if(p)if(p[2])var
f=0,e=0;else
var
j=[7,o[1],p[1]],e=1;else
var
f=0,e=0}else
var
f=0,e=0}else{var
q=g[2];if(q)if(q[2])var
f=0,e=0;else
var
j=[6,q[1]],e=1;else
var
f=0,e=0}else{var
r=g[2];if(r){var
s=r[2];if(s)if(s[2])var
f=0,e=0;else
var
j=[10,r[1],s[1]],e=1;else
var
f=0,e=0}else
var
f=0,e=0}else{var
t=g[2];if(t){var
v=t[2];if(v)if(v[2])var
f=0,e=0;else
var
j=[8,t[1],v[1]],e=1;else
var
f=0,e=0}else
var
f=0,e=0}else
if(g[2])var
f=0,e=0;else
var
j=0,e=1;else
if(g[2])var
f=0,e=0;else
var
j=1,e=1;if(e)var
m=j,f=1;break;case
2:var
m=[9,g[1],g[2]],f=1;break;default:var
f=0}if(!f)var
m=2;var
K=m}catch(b){b=u(b);if(!a(av[4],b))throw b;var
K=2}var
L=K}return L}catch(b){b=u(b);if(a(av[4],b))return 2;throw b}}var
c=[0,c9,c$,db,dd,df,dh,dj,dl,dn,dq,ds,du,dw,dy,dA,dC,dE,dG,dI,dK,dM,dO,dQ,dS,dU,dW,dY,d0,d2,d4,d6,d8,d_,eb,ed,ef,eh,aj,ak,al,am,q,eo,eq,es,eu,ew,ey,eA,eC,eE,eG,eI,eK,eM,eO,eQ,eS,eU,eW,eY,e0,e2,e4,e6,e8,e_,fa,fc,fe,fg,fi,fk,fm,fo,fq,fs,fu,fw,fy,fA,fC,fE,fG,fI,fK,fM,bx,by,fN,bz,bA,bB,bE,fW,fY,[0,f6,f8,f_,ga,gc,bG,gr,gB,function(b){function
j(l){var
e=l;for(;;){var
c=ai(e);if(typeof
c!=="number"&&1===c[0]){var
a=c[1];if(n(a,gJ))if(n(a,gK)){if(n(a,gL))if(n(a,gM))if(n(a,gN))if(n(a,gO)){if(n(a,gP))if(n(a,gQ))if(n(a,gR))var
b=1,d=0,f=0;else
var
f=1;else
var
f=1;else
var
f=1;if(f){bF(e);return 1}}else
var
d=1;else
var
b=0,d=0;else
var
d=1;else
var
d=1;if(d){var
g=c[2];if(g){if(!g[2]){var
e=g[1];continue}var
b=1}else
var
b=1}}else
var
b=0;else
var
b=0;if(!b){var
h=c[2];if(h){var
i=h[2];if(i)if(!i[2]){var
m=i[1],k=j(h[1]);if(k){var
e=m;continue}return k}}}}return 0}}try{var
c=j(b);return c}catch(b){b=u(b);if(a(J[22],b))return 0;throw b}}]];aV(289,c,"Romega_plugin.Const_omega");var
o=a(gU[1][2],[0,r[17],r[16],r[12],r[13],r[14],r[15],r[22],r[5],r[6],r[2]]),L=[0,0];function
bI(b){L[1];return a(ac[1],b)}function
gW(b){a(k[29],b);a(k[32],0);return a(k[46],k[24])}var
ad=ac[5],A=s[G];function
bJ(c,a){switch(c){case
0:var
b=0===a?1:0;break;case
1:var
b=1===a?1:0;break;default:var
b=2<=a?1:0}return b?1:0}var
V=a(v[1][5],gX);function
bK(c){var
a=d(E[1],0,7),b=d(E[1],0,7);return[0,0,0,0,d(E[1],0,7),0,b,a]}function
bL(a){a[5]=a[5]+1|0;return a[5]}function
bM(a){return 0===a[0]?[1,a[1]]:[0,a[1]]}function
gY(a){return a[1]}function
bN(c){function
b(f,e,c){if(c){var
g=c[2],h=c[1],i=t(l[4],gZ,f,e),j=b(f,e+1|0,g),k=a(w[6],0),m=a(gT[2],h),n=a(w[1],g0),o=a(w[1],i),p=a(w[16],0),q=d(w[13],p,o),r=d(w[13],q,n),s=d(w[13],r,m),u=d(w[13],s,k);return d(w[13],u,j)}return a(w[1],g1)}var
e=b(80,0,c[2]),f=a(w[6],0),g=a(w[1],g2),h=d(w[13],g,f),i=d(w[13],h,e),j=b(86,0,c[1]),k=a(w[6],0),m=a(w[1],g3),n=d(w[13],m,k),o=d(w[13],n,j),p=a(w[6],0),q=d(w[13],i,p),r=d(w[13],q,o);return d(gV[16],0,r)}var
a6=[0,0];function
bO(a){a6[1]=0;return 0}function
a7(a){a6[1]++;return a6[1]}var
a8=[0,0];function
bP(a){a8[1]=0;return 0}function
a9(a){a8[1]++;return a8[1]}function
an(a){return d(l[4],g4,a)}function
a_(b,c){try{var
a=t(f[17][bt],_,c,b[3]);return a}catch(a){a=u(a);if(a===H){var
d=a9(0);b[3]=[0,[0,c,d],b[3]];return d}throw a}}function
bQ(a,c,b){a[3]=[0,[0,c,b],a[3]];return 0}function
aG(e,d){var
b=e[3];for(;;){if(b){var
c=b[1],f=b[2],g=c[1];if(d===c[2])return g;var
b=f;continue}return a(k[2],g5)}}function
a$(e,b){try{var
c=t(f[17][79],s[S],e,b[1]);return c}catch(c){c=u(c);if(c===H){var
g=a(f[17][1],b[1]);b[1]=d(f[18],b[1],[0,e,0]);return g}throw c}}function
ba(b){try{var
c=a(f[17][5],b[1]);return c}catch(b){b=u(b);if(b[1]===bH)return a(k[2],g6);throw b}}function
bR(b,e){try{var
c=t(f[17][79],s[S],e,b[2]);return c}catch(c){c=u(c);if(c===H){var
g=a(f[17][1],b[2]);b[2]=d(f[18],b[2],[0,e,0]);return g}throw c}}function
g7(c,b){try{var
e=d(f[17][5],c,b);return e}catch(b){b=u(b);if(b[1]===bH)return a(k[2],g8);throw b}}function
bS(c,b){var
e=b[8][1];try{d(E[6],c[6],e);var
a=0;return a}catch(a){a=u(a);if(a===H)return t(E[5],c[6],e,b);throw a}}function
bb(a,b){try{var
c=d(E[6],a[6],b);return c}catch(a){a=u(a);if(a===H){d(l[2],g9,b);throw a}throw a}}function
B(c,b){switch(b[0]){case
0:var
e=a(r[2],b[1]);return t(l[1],c,g_,e);case
1:return ah(l[1],c,g$,B,b[1],B,b[2]);case
2:return ah(l[1],c,ha,B,b[1],B,b[2]);case
3:return ah(l[1],c,hb,B,b[1],B,b[2]);case
4:return bn(l[1],c,hc,B,b[1]);case
5:return t(l[1],c,hd,b[1]);default:return d(l[1],c,he)}}function
F(b,a){if(typeof
a==="number")return 0===a?d(l[1],b,hf):d(l[1],b,hg);else
switch(a[0]){case
0:var
e=a[2],f=e[3],g=e[2];switch(e[1]){case
0:var
c=hh;break;case
1:var
c=hj;break;case
2:var
c=hk;break;case
3:var
c=hl;break;case
4:var
c=hm;break;default:var
c=hn}return cb(l[1],b,hi,B,g,c,B,f);case
1:return bn(l[1],b,ho,F,a[1]);case
2:return ah(l[1],b,hp,F,a[2],F,a[3]);case
3:return ah(l[1],b,hq,F,a[2],F,a[3]);case
4:return ah(l[1],b,hr,F,a[2],F,a[3]);default:return d(l[1],b,hs)}}function
C(d,c){var
b=c;for(;;)switch(b[0]){case
0:return-1;case
1:return a(k[2],ht);case
2:var
b=b[1];continue;case
3:return a(k[2],hu);case
4:var
b=b[1];continue;case
5:return a_(d,b);default:return-1}}function
bT(h,g){var
n=0;return function(o){var
c=n,b=o;for(;;){switch(b[0]){case
0:var
i=b[1],j=a7(0);return[0,j,g,a(f[17][6],c),i];case
1:var
d=b[1];if(2===d[0]){var
e=d[2];if(0===e[0]){var
l=b[2],m=e[1],c=[0,[0,m,a_(h,d[1])],c],b=l;continue}}break}a(k[27],hv);B(k[24],b);return a(k[2],hw)}}}function
aH(e,b){function
c(a){if(a){var
d=a[1],f=d[2],g=d[1],h=c(a[2]);return[1,[2,aG(e,f),[0,g]],h]}return[0,b[4]]}return c(b[3])}function
m(b,d){var
c=i(b),f=j===c?b[1]:e===c?a(g[2],b):b;return a(A,[0,f,d])}function
bU(e,b){function
d(f){var
b=f;for(;;)switch(b[0]){case
0:return a(c[97][6],b[1]);case
1:var
g=b[1],h=d(b[2]),i=[0,d(g),h];return m(c[97][2],i);case
2:var
j=b[1],k=d(b[2]),l=[0,d(j),k];return m(c[97][3],l);case
3:var
n=b[1],o=d(b[2]),p=[0,d(n),o];return m(c[97][5],p);case
4:var
q=[0,d(b[1])];return m(c[97][4],q);case
5:var
r=b[1];return a(ba(e),r);default:var
b=b[1];continue}}return d(b)}function
bV(c,b){try{var
a=d(E[6],c[4],b);return a}catch(a){a=u(a);if(a===H){d(l[2],hx,b);var
e=c[4],f=function(b,a){return t(l[2],hy,b,a)};d(E[11],f,e);throw H}throw a}}function
N(d,e){var
b=e;for(;;)switch(b[0]){case
0:var
f=[0,a(c[97][6],b[1])];return m(c[14],f);case
1:var
g=b[1],h=N(d,b[2]),i=[0,N(d,g),h];return m(c[15],i);case
2:var
j=b[1],k=N(d,b[2]),l=[0,N(d,j),k];return m(c[16],l);case
3:var
n=b[1],o=N(d,b[2]),p=[0,N(d,n),o];return m(c[18],p);case
4:var
q=[0,N(d,b[1])];return m(c[17],q);case
5:var
r=bV(d,b[1]),s=[0,a(c[93],r)];return m(c[19],s);default:var
b=b[1];continue}}function
z(a,b){try{var
c=N(a,b);return c}catch(a){a=u(a);B(k[25],b);throw a}}function
O(b,f){if(typeof
f==="number"){if(0===f){var
h=c[27],l=i(h);return j===l?h[1]:e===l?a(g[2],h):h}var
k=c[28],n=i(k);return j===n?k[1]:e===n?a(g[2],k):k}else
switch(f[0]){case
0:var
d=f[2];switch(d[1]){case
0:var
o=d[2],p=z(b,d[3]),q=[0,z(b,o),p];return m(c[21],q);case
1:var
r=d[2],s=z(b,d[3]),t=[0,z(b,r),s];return m(c[22],t);case
2:var
u=d[2],v=z(b,d[3]),w=[0,z(b,u),v];return m(c[23],w);case
3:var
x=d[2],y=z(b,d[3]),A=[0,z(b,x),y];return m(c[25],A);case
4:var
B=d[2],C=z(b,d[3]),D=[0,z(b,B),C];return m(c[24],D);default:var
E=d[2],F=z(b,d[3]),G=[0,z(b,E),F];return m(c[26],G)}case
1:var
H=[0,O(b,f[1])];return m(c[29],H);case
2:var
I=f[2],J=O(b,f[3]),K=[0,O(b,I),J];return m(c[30],K);case
3:var
L=f[2],M=O(b,f[3]),N=[0,O(b,L),M];return m(c[31],N);case
4:var
P=f[2],Q=O(b,f[3]),R=[0,O(b,P),Q];return m(c[32],R);default:var
S=bR(b,f[1]),T=[0,a(c[93],S)];return m(c[33],T)}}function
aI(a,b){try{var
c=O(a,b);return c}catch(a){a=u(a);F(k[25],b);throw a}}function
hz(b,e,d){var
g=[0,a(c[97][6],d)],h=m(c[14],g);function
i(d,e){var
f=d[2],g=[0,a(c[97][6],d[1])],h=m(c[14],g),i=[0,z(b,aG(b,f)),h],j=[0,m(c[16],i),e];return m(c[15],j)}return t(f[17][16],i,e,h)}function
aJ(a,c,b){try{var
e=hz(a,c,b);return e}catch(a){a=u(a);d(o[29],an,[0,c,b]);throw a}}var
D=a(f[17][97],y.caml_int_compare);function
M(b){var
a=b;for(;;)switch(a[0]){case
0:return 0;case
1:var
c=a[1],e=M(a[2]);return d(D,M(c),e);case
2:var
f=a[1],g=M(a[2]);return d(D,M(f),g);case
3:var
h=a[1],i=M(a[2]);return d(D,M(h),i);case
4:var
a=a[1];continue;case
5:return[0,a[1],0];default:return 0}}function
aK(a){if(a){var
b=a[1],c=aK(a[2]),e=d(D,M(b[3]),c);return d(D,M(b[2]),e)}return 0}function
P(b){var
a=b;for(;;){if(typeof
a!=="number")switch(a[0]){case
0:return aK([0,a[2],0]);case
1:var
a=a[1];continue;case
2:var
c=a[2],e=P(a[3]);return d(D,P(c),e);case
3:var
f=a[2],g=P(a[3]);return d(D,P(f),g);case
4:var
h=a[2],i=P(a[3]);return d(D,P(h),i)}return 0}}function
ao(f,b){switch(b[0]){case
0:var
x=[0,d(o[8],f,b[1])],h=c[47],p=i(h),y=0,z=j===p?h[1]:e===p?a(g[2],h):h;return[0,a(c[92],[0,z,y]),x];case
1:var
A=b[2],q=ao(f,b[1]),B=q[2],C=q[1],s=ao(f,A),D=[1,B,s[2]],E=[0,d(c[90],C,s[1]),0],l=c[48],t=i(l),F=j===t?l[1]:e===t?a(g[2],l):l;return[0,a(c[92],[0,F,E]),D];case
2:var
u=b[2],G=b[1];if(0===u[0]){var
H=[2,G,[0,d(o[8],f,u[1])]],m=c[63],v=i(m),I=0,K=j===v?m[1]:e===v?a(g[2],m):m;return[0,a(c[92],[0,K,I]),H]}return a(J[6],hA);case
3:return a(k[2],hB);case
4:var
L=b[1],M=[2,L,[0,a(r[22],f)]],n=c[62],w=i(n),N=0,O=j===w?n[1]:e===w?a(g[2],n):n;return[0,a(c[92],[0,O,N]),M];case
5:return[0,a(c[92],0),[2,b,[0,f]]];default:return[0,a(c[92],0),[6,[2,b,[0,f]]]]}}function
aL(b){switch(b[0]){case
0:var
y=[0,a(r[22],b[1])],f=c[47],p=i(f),z=0,A=j===p?f[1]:e===p?a(g[2],f):f;return[0,a(c[92],[0,A,z]),y];case
1:var
B=b[2],q=aL(b[1]),C=q[2],D=q[1],s=aL(B),E=[1,C,s[2]],F=[0,d(c[90],D,s[1]),0],h=c[43],t=i(h),G=j===t?h[1]:e===t?a(g[2],h):h;return[0,a(c[92],[0,G,F]),E];case
2:var
u=b[2],H=b[1];if(0===u[0]){var
I=[2,H,[0,a(r[22],u[1])]],l=c[45],v=i(l),K=0,L=j===v?l[1]:e===v?a(g[2],l):l;return[0,a(c[92],[0,L,K]),I]}return a(J[6],hC);case
3:return a(k[2],hD);case
4:var
m=c[44],w=i(m),M=b[1],N=0,O=j===w?m[1]:e===w?a(g[2],m):m;return[0,a(c[92],[0,O,N]),M];case
5:var
n=c[46],x=i(n),P=[2,b,[0,o[14]]],Q=0,R=j===x?n[1]:e===x?a(g[2],n):n;return[0,a(c[92],[0,R,Q]),P];default:var
S=[6,[4,b[1]]];return[0,a(c[92],0),S]}}function
hE(b){return a(f[17][1],b)}function
bW(I,h,H,f){function
b(u){var
f=u[1];if(f){var
h=u[2],l=f[2],w=f[1],x=w[2],J=w[1];if(h){var
v=h[2],y=h[1],z=y[2],K=y[1];if(x===z){var
L=o[11],M=d(o[8],H,K),N=d(o[8],I,J),O=d(o[6],N,M);if(d(r[17],O,L)){var
P=b([0,l,v]),m=c[35],A=i(m),Q=j===A?m[1]:e===A?a(g[2],m):m;return[0,Q,P]}var
R=b([0,l,v]),n=c[34],B=i(n),S=j===B?n[1]:e===B?a(g[2],n):n;return[0,S,R]}if(d(o[19],x,z)){var
T=b([0,l,h]),p=c[36],C=i(p),U=j===C?p[1]:e===C?a(g[2],p):p;return[0,U,T]}var
V=b([0,f,v]),q=c[37],D=i(q),W=j===D?q[1]:e===D?a(g[2],q):q;return[0,W,V]}var
X=b([0,l,0]),s=c[36],E=i(s),Y=j===E?s[1]:e===E?a(g[2],s):s;return[0,Y,X]}var
F=u[2];if(F){var
Z=b([0,0,F[2]]),t=c[37],G=i(t),_=j===G?t[1]:e===G?a(g[2],t):t;return[0,_,Z]}a(k[46],k[24]);return 0}var
l=b([0,h,f]);return a(c[96],l)}function
ae(f,u){var
b=u[2],h=u[1];switch(h[0]){case
0:var
K=h[1];switch(b[0]){case
0:var
L=[0,d(o[6],K,b[1])],k=c[47],v=i(k),M=0,N=j===v?k[1]:e===v?a(g[2],k):k;return[0,a(c[92],[0,N,M]),L];case
1:var
t=0;break;default:var
t=1}break;case
1:var
A=h[2],n=h[1];if(1===b[0]){var
F=b[1],ag=b[2],ah=C(f,F),ai=C(f,n);if(d(o[19],ai,ah)){var
G=ae(f,[0,A,b]),aj=[1,n,G[2]],ak=[0,a(c[89],G[1]),0],r=c[51],H=i(r),al=j===H?r[1]:e===H?a(g[2],r):r;return[0,a(c[92],[0,al,ak]),aj]}var
I=ae(f,[0,h,ag]),am=[1,F,I[2]],an=[0,a(c[89],I[1]),0],s=c[53],J=i(s),ao=j===J?s[1]:e===J?a(g[2],s):s;return[0,a(c[92],[0,ao,an]),am]}var
Z=C(f,b),_=C(f,n);if(d(o[19],_,Z)){var
B=ae(f,[0,A,b]),$=[1,n,B[2]],aa=[0,a(c[89],B[1]),0],p=c[51],D=i(p),ab=j===D?p[1]:e===D?a(g[2],p):p;return[0,a(c[92],[0,ab,aa]),$]}var
q=c[54],E=i(q),ac=[1,b,h],ad=0,af=j===E?q[1]:e===E?a(g[2],q):q;return[0,a(c[92],[0,af,ad]),ac];default:var
t=0}if(!t)if(1===b[0]){var
x=b[1],T=b[2],U=C(f,h),V=C(f,x);if(d(o[19],V,U)){var
y=ae(f,[0,h,T]),W=[1,x,y[2]],X=[0,a(c[89],y[1]),0],m=c[53],z=i(m),Y=j===z?m[1]:e===z?a(g[2],m):m;return[0,a(c[92],[0,Y,X]),W]}return[0,a(c[92],0),[1,h,b]]}var
O=C(f,b),P=C(f,h);if(d(o[18],P,O)){var
l=c[54],w=i(l),Q=[1,b,h],R=0,S=j===w?l[1]:e===w?a(g[2],l):l;return[0,a(c[92],[0,S,R]),Q]}return[0,a(c[92],0),[1,h,b]]}function
bc(d,b){switch(d[0]){case
2:var
n=d[1],u=d[2];switch(b[0]){case
2:if(5===n[0]){var
f=c[59],p=i(f),v=[2,[5,n[1]],[1,d[2],b[2]]],w=j===p?f[1]:e===p?a(g[2],f):f;return[0,w,v]}break;case
5:var
h=c[58],q=i(h),x=[2,[5,b[1]],[1,u,[0,o[12]]]],y=j===q?h[1]:e===q?a(g[2],h):h;return[0,y,x]}break;case
5:var
r=d[1];switch(b[0]){case
2:var
l=c[57],s=i(l),z=[2,[5,r],[1,b[2],[0,o[12]]]],A=j===s?l[1]:e===s?a(g[2],l):l;return[0,A,z];case
5:var
m=c[56],t=i(m),C=[2,[5,r],[0,o[13]]],D=j===t?m[1]:e===t?a(g[2],m):m;return[0,D,C]}break}B(k[24],d);a(k[32],0);B(k[24],b);a(k[32],0);a(k[46],k[24]);return a(J[6],hF)}function
ap(b){switch(b[0]){case
2:var
l=b[1];if(5===l[0]){var
m=b[2],q=l[1];if(0===m[0])return[0,0,b];var
k=function(b){switch(b[0]){case
0:return b[1];case
1:var
c=b[1],e=k(b[2]),f=k(c);return d(o[6],f,e);default:return a(J[6],hH)}},r=[2,[5,q],[0,k(m)]],f=c[47],n=i(f),s=0,t=j===n?f[1]:e===n?a(g[2],f):f;return[0,[0,t,s],r]}break;case
5:var
h=c[55],p=i(h),u=[2,[5,b[1]],[0,o[12]]],v=0,w=j===p?h[1]:e===p?a(g[2],h):h;return[0,[0,w,v],u]}return a(J[6],hG)}function
af(b,l){switch(l[0]){case
0:return[0,0,l];case
1:var
h=l[2],k=l[1];switch(h[0]){case
0:var
D=h[1],p=ap(k),E=[1,p[2],[0,D]],F=a(c[92],p[1]);return[0,[0,a(c[88],F),0],E];case
1:var
q=h[1],G=h[2],H=C(b,q);if(C(b,k)===H){var
r=bc(k,q),m=c[52],s=i(m),I=r[2],J=r[1],K=j===s?m[1]:e===s?a(g[2],m):m,t=af(b,[1,I,G]),L=t[2],M=t[1],N=a(c[92],[0,J,0]);return[0,[0,K,[0,a(c[88],N),M]],L]}var
u=ap(k),O=u[2],P=u[1],v=af(b,h),Q=[1,O,v[2]],R=a(c[92],v[1]),S=a(c[92],P);return[0,[0,d(c[90],S,R),0],Q];default:var
T=C(b,h);if(C(b,k)===T){var
w=bc(k,h),U=w[1],x=af(b,w[2]);return[0,[0,U,x[1]],x[2]]}var
y=ap(k),V=y[2],W=y[1],z=af(b,h),X=[1,V,z[2]],Y=a(c[92],z[1]),Z=a(c[92],W);return[0,[0,d(c[90],Z,Y),0],X]}default:var
A=ap(l),n=c[61],B=i(n),_=A[1],$=[1,A[2],[0,o[11]]],aa=0,ab=j===B?n[1]:e===B?a(g[2],n):n;return[0,d(f[18],_,[0,ab,aa]),$]}}function
aM(b){if(1===b[0]){var
h=b[1];if(2===h[0])if(5===h[1][0]){var
p=h[2];if(0===p[0]){var
v=b[2];if(d(r[17],p[1],o[11])){var
q=aM(v),k=c[60],s=i(k),w=q[2],x=q[1],y=j===s?k[1]:e===s?a(g[2],k):k;return[0,[0,y,x],w]}}}var
l=aM(b[2]),m=l[1],t=[1,h,l[2]];if(a(f[17][47],m))var
n=0;else
var
u=a(c[92],m),n=[0,a(c[89],u),0];return[0,n,t]}return[0,0,b]}function
Q(f,b){switch(b[0]){case
1:var
G=b[2],q=Q(f,b[1]),H=q[2],I=q[1],r=Q(f,G),J=r[2],s=ae(f,[0,I,r[1]]),K=s[2],L=[0,s[1],0],M=[0,d(c[90],H,J),L];return[0,K,a(c[92],M)];case
2:var
N=b[2],t=Q(f,b[1]),u=t[2],n=t[1],v=Q(f,N),w=v[2],o=v[1];if(0===o[0]){var
x=ao(o[1],n),O=x[2],P=[0,x[1],0],R=[0,d(c[90],u,w),P];return[0,O,a(c[92],R)]}if(0===n[0]){var
y=ao(n[1],o),k=c[65],z=i(k),S=y[2],T=[0,y[1],0],U=j===z?k[1]:e===z?a(g[2],k):k,V=[0,d(c[90],u,w),[0,U,T]];return[0,S,a(c[92],V)]}var
l=c[42],A=i(l),W=j===A?l[1]:e===A?a(g[2],l):l;return[0,[6,b],W];case
3:var
B=Q(f,[1,b[1],[4,b[2]]]),m=c[64],C=i(m),X=B[1],Y=[0,B[2],0],Z=j===C?m[1]:e===C?a(g[2],m):m;return[0,X,a(c[92],[0,Z,Y])];case
4:var
D=Q(f,b[1]),_=D[2],E=aL(D[1]),$=E[2],aa=[0,E[1],0],ab=[0,a(c[88],_),aa];return[0,$,a(c[92],ab)];default:var
h=c[42],p=i(h),F=j===p?h[1]:e===p?a(g[2],h):h;return[0,b,F]}}function
bd(b,g){var
d=Q(b,g),h=d[2],e=af(b,d[1]),i=e[1],f=aM(e[2]),j=f[2],k=[0,a(c[92],f[1]),0],l=[0,h,[0,a(c[92],i),k]];return[0,a(c[92],l),j]}function
bX(a){switch(a){case
0:return 5;case
1:return 3;case
2:return 4;case
3:return 1;case
4:return 2;default:return 0}}function
bY(k,h,i){var
b=i[3],c=i[2],j=i[1],l=h[1],m=h[4],n=h[3],p=h[2];function
e(c,b,h,g){var
e=bd(k,d(h,c,b)),i=e[2],o=e[1],q=a(bT(k,g),i);return[0,j,c,b,o,[0,n,a(f[17][6],m)],l,p,q]}try{var
q=l?bX(j):j;switch(q){case
0:var
r=0,g=e(c,b,function(b,a){return[1,b,[4,a]]},r);break;case
1:var
s=1,g=e(c,b,function(b,a){return[1,a,[4,b]]},s);break;case
2:var
t=1,g=e(c,b,function(b,a){return[1,b,[4,a]]},t);break;case
3:var
v=1,g=e(c,b,function(b,a){return[1,[1,b,[0,o[14]]],[4,a]]},v);break;case
4:var
w=1,g=e(c,b,function(b,a){return[1,[1,a,[0,o[14]]],[4,b]]},w);break;default:var
x=2,g=e(c,b,function(b,a){return[1,b,[4,a]]},x)}return g}catch(b){b=u(b);if(a(av[4],b))throw b;throw b}}function
aN(a,e,c,b){var
f=W(a,c);return d(e,f,W(a,b))}function
W(d,e){var
b=a(c[97][7],e);if(typeof
b!=="number")switch(b[0]){case
0:var
h=b[2],i=b[1];return aN(d,function(b,a){return[1,b,a]},i,h);case
1:var
f=b[2],g=b[1],m=a(c[97][9],g)?0:a(c[97][9],f)?0:1;if(!m)return aN(d,function(b,a){return[2,b,a]},g,f);break;case
2:var
j=b[2],k=b[1];return aN(d,function(b,a){return[3,b,a]},k,j);case
3:return[4,W(d,b[1])];case
4:var
l=[0,o[12]];return[1,W(d,b[1]),l];default:return[0,b[1]]}return[5,a$(e,d)]}function
aq(c,b,g,n,j,m,l,k){var
h=b[4],i=b[3],d=b[2],o=b[1],e=bL(c),p=g?[0,[0,e],d]:d,q=g?[0,[1,e],d]:d;if(g){var
r=[0,i,a(f[17][6],h)];t(E[5],c[7],e,r)}var
s=ag(c,[0,n,p,i,[0,0,h]],j,l);return t(m,e,s,ag(c,[0,o,q,i,[0,1,h]],j,k))}function
X(a,g,f,e,d,c){var
h=W(a,d),b=bY(a,g,[0,e,h,W(a,c)]);bS(a,b);return[0,f,b]}function
ag(e,b,h,g){var
f=b[1],k=b[4],l=b[3],m=b[2],a=d(c[97][8],h,g);if(typeof
a==="number")switch(a){case
0:return 0;case
1:return 1;default:return[5,g]}else
switch(a[0]){case
0:return X(e,b,g,0,a[1],a[2]);case
1:return X(e,b,g,5,a[1],a[2]);case
2:return X(e,b,g,4,a[1],a[2]);case
3:return X(e,b,g,1,a[1],a[2]);case
4:return X(e,b,g,3,a[1],a[2]);case
5:return X(e,b,g,2,a[1],a[2]);case
6:return[1,ag(e,[0,1-f,m,l,[0,2,k]],h,a[1])];case
7:var
n=a[2],o=a[1];return aq(e,b,1-f,f,h,function(c,b,a){return[2,c,b,a]},o,n);case
8:var
p=a[2],q=a[1];return aq(e,b,f,f,h,function(c,b,a){return[3,c,b,a]},q,p);case
9:var
r=a[2],t=a[1];return aq(e,b,1-f,1-f,h,function(c,b,a){return[4,c,b,a]},t,r);default:var
i=a[2],j=a[1],u=d(s[49],i,j),v=d(s[49],j,i);return aq(e,b,f,f,h,function(c,b,a){return[3,c,b,a]},v,u)}}function
bZ(c,b){var
e=[1,ag(c,[0,1,0,V,hI],b,a(a2[7],b))];if(L[1]){a(l[2],hJ);a(l[2],hK);F(k[24],e);a(l[2],hL)}function
g(e){if(e){var
h=e[1],f=h[1],j=e[2],i=ag(c,[0,0,0,f,0],b,h[2]);if(L[1]){var
m=a(v[1][7],f);d(l[2],hM,m);F(k[24],i);a(l[2],hN)}return[0,[0,f,i],g(j)]}if(L[1])bN(c);return 0}return[0,[0,V,e],g(a(a2[10],b))]}function
bm(k,e,b,c,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,[0,a[2],b],0];case
1:var
g=a[1];return k<50?bl(k+1|0,e,b,c,g):cc(bl,[0,e,b,c,g]);case
2:var
h=a[1],l=a[3],m=R(e,b,[0,h,c],a[2]),n=R(e,b,[0,h,c],l);return d(f[18],m,n);case
3:var
o=a[3],p=R(e,b,c,a[2]),i=function(a){if(a){var
b=a[1],g=i(a[2]),h=R(e,b,c,o);return d(f[18],h,g)}return 0};return i(p);case
4:var
j=a[1],q=a[3],r=Y(e,b,[0,j,c],a[2]),s=R(e,b,[0,j,c],q);return d(f[18],r,s)}return[0,b,0]}function
bl(k,e,c,b,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,[0,a[2],c],0];case
1:var
g=a[1];return k<50?bm(k+1|0,e,c,b,g):cc(bm,[0,e,c,b,g]);case
2:var
l=a[3],m=Y(e,c,b,a[2]),h=function(a){if(a){var
c=a[1],g=h(a[2]),i=Y(e,c,b,l);return d(f[18],i,g)}return 0};return h(m);case
3:var
i=a[1],n=a[3],o=Y(e,c,[0,i,b],a[2]),p=Y(e,c,[0,i,b],n);return d(f[18],o,p);case
4:var
q=a[3],r=R(e,c,b,a[2]),j=function(a){if(a){var
c=a[1],g=j(a[2]),h=Y(e,c,b,q);return d(f[18],h,g)}return 0};return j(r)}return[0,c,0]}function
R(a,b,c,d){return cd(bm(0,a,b,c,d))}function
Y(a,b,c,d){return cd(bl(0,a,b,c,d))}function
b0(a){function
b(a){if(a){var
c=a[1],d=a[2],e=R(c[1],0,0,c[2]),g=b(d);return t(f[17][122],f[18],e,g)}return hO}return b(a)}function
aO(a){return 0===a[0]?d(l[2],hP,a[1]):d(l[2],hQ,a[1])}function
b1(b){function
h(b){var
h=c[42],n=i(h),s=j===n?h[1]:e===n?a(g[2],h):h;F(k[24],[0,s,b]);a(k[32],0);var
m=b[8],p=a(o[31],m[2]),q=[0,m[3],m[4]];function
r(b){return a(o[29],an)}iX(l[2],hR,m[1],r,q,p);a(l[2],hS);d(f[17][11],aO,b[7]);var
u=b[5][2];function
w(a){switch(a){case
0:return hT;case
1:return hU;default:return hV}}var
x=d(f[17][12],w,u),y=d(f[15][7],hW,x);d(l[2],hX,y);var
z=b[6]?hY:h0,A=a(v[1][7],b[5][1]);return t(l[2],hZ,A,z)}function
m(b){a(l[2],h1);return d(f[17][11],h,b)}return d(f[17][11],m,b)}function
ar(e){var
a=e;for(;;){if(a){var
c=a[2],b=a[1];switch(b[0]){case
6:var
f=b[1],g=ar(c);return d(D,[0,f[1],0],g);case
15:var
h=b[2][2],i=ar(b[3][2]);return d(D,ar(h),i);default:var
a=c;continue}}return 0}}function
as(e){var
a=e;for(;;){if(a){var
c=a[2],b=a[1];switch(b[0]){case
5:var
g=b[1];return[0,g,as(c)];case
15:var
h=b[2][2],i=as(b[3][2]),j=as(h);return d(f[18],j,i);default:var
a=c;continue}}return 0}}function
b2(b,l){function
k(b,a){return b[5]-a[5]|0}function
h(a){if(0===a[0]){var
b=as(a[1][3]);return d(f[17][40],k,b)}var
c=a[2],e=h(a[3]),g=h(c);return t(f[17][44],k,g,e)}var
n=h(l);function
o(f){var
k=aH(b,f[2]),l=bU(b,k),h=a$(l,b),d=c[97][1],n=i(d),o=j===n?d[1]:e===n?a(g[2],d):d,p=m(c[1],[0,o,l]);bQ(b,[5,h],f[5]);return[0,h,p,[0,k,[5,h]],f[2][1]]}return d(f[17][12],o,n)}function
be(c,b){if(b){var
e=b[2],g=b[1];try{var
j=bb(c,g)[7],d=j}catch(a){a=u(a);if(a!==H)throw a;var
d=0}var
h=be(c,e),i=a(f[17][6],d);return t(f[17][53],_,i,h)}return 0}function
b3(b){function
c(c,b){var
d=c[2],e=a(f[17][1],b[2]);return a(f[17][1],d)-e|0}try{var
e=d(f[17][40],c,b),g=a(f[17][3],e);return g}catch(b){b=u(b);if(b[1]===gS)return a(k[2],h2);throw b}}function
b4(c,a){function
e(h){var
a=h;for(;;){if(a){var
g=a[2],b=a[1];if(d(f[17][26],b,c)){var
a=g;continue}var
i=bM(b);if(d(f[17][26],i,c))throw k[3];return[0,b,e(g)]}return 0}}function
b(a){var
b=a[2],c=a[1];try{var
d=[0,[0,c,e(b)]];return d}catch(a){a=u(a);if(a===k[3])return 0;throw a}}return d(f[17][64],b,a)}function
aP(a){if(0===a[0])return a[1][2];var
b=a[2],c=aP(a[3]);return d(D,aP(b),c)}function
b5(j,h){function
b(a){if(typeof
a==="number")return 0===a?m(c[5],[0]):m(c[6],[0]);else
switch(a[0]){case
0:return a[1];case
1:var
e=[0,b(a[1])];return m(c[3],e);case
2:var
f=a[2],g=b(a[3]),h=[0,b(f),g];return m(c[4],h);case
3:var
i=a[2],j=b(a[3]),k=[0,b(i),j];return m(c[2],k);case
4:var
l=a[2],n=b(a[3]),o=b(l);return d(s[49],o,n);default:return a[1]}}function
g(d,g,f){var
h=e(g),c=e(f);if(h){var
i=h[1];return c?[0,a(d,[0,i,c[1]])]:[0,a(d,[0,i,[5,b(f)]])]}if(c){var
j=c[1];return[0,a(d,[0,[5,b(g)],j])]}return 0}function
e(a){if(typeof
a==="number")return 0===a?0:0;else
switch(a[0]){case
0:return d(f[17][26],a[2][8][1],j)?[0,a]:0;case
1:var
b=e(a[1]);return b?[0,[1,b[1]]]:0;case
2:var
c=a[3],h=a[2],i=a[1];return g(function(a){return[2,i,a[1],a[2]]},h,c);case
3:var
k=a[3],l=a[2],m=a[1];return g(function(a){return[3,m,a[1],a[2]]},l,k);case
4:var
n=a[3],o=a[2],p=a[1];return g(function(a){return[4,p,a[1],a[2]]},o,n);default:return 0}}var
i=e(h);return i?i[1]:[5,b(h)]}function
aQ(b,a){if(0===a[0]){var
c=a[1],e=d(f[17][12],k[20],c[2]),g=d(f[15][7],h3,e),h=t(l[4],h4,c[1],g);return d(k[49],b,h)}return cb(l[1],b,h5,a[1],aQ,a[2],aQ,a[3])}function
aR(b,c){function
e(g,d,c){if(c){var
h=c[1];if(0===h[0]){var
i=h[1],k=c[2],l=aR(b,a(f[17][6],[0,[1,i],d]));return[1,i,e(g,[0,[0,i],d],k),l]}var
j=h[1],m=e(g,[0,[1,j],d],c[2]);return[1,j,aR(b,a(f[17][6],[0,[0,j],d])),m]}return[0,g]}var
g=b4(c,b);try{var
h=b3(g)}catch(e){e=u(e);var
i=a(f[17][1],b),j=a(f[17][1],g);t(l[2],h6,j,i);d(f[17][11],aO,c);throw e}var
k=h[2],m=h[1];return e(m,a(f[17][6],c),k)}function
bf(i,m){var
c=0,b=m,n=i[2],o=i[1];a:for(;;){if(b){var
j=b[1];if(0===j[0]){var
l=j[1],r=b[2],s=l[2];if(d(v[1][1],o,l[1])){var
e=[0,s,n];for(;;){var
f=e[1];if(f){var
g=e[2];if(g){var
p=g[2],q=f[2];if(bJ(f[1],g[1])){var
e=[0,q,p];continue}}var
h=0}else
var
h=[0,e[2]];if(h)return[0,c,h[1]];var
c=c+1|0,b=r;continue a}}}var
c=c+1|0,b=b[2];continue}return a(k[2],h7)}}function
bg(k){function
l(f){switch(f){case
0:var
b=c[80];break;case
1:var
b=c[81];break;default:var
b=c[82]}var
d=i(b);return j===d?b[1]:e===d?a(g[2],b):b}var
m=d(f[17][12],l,k),b=c[79],h=i(b),n=j===h?b[1]:e===h?a(g[2],b):b;return d(c[94],n,m)}function
x(b,c){try{var
g=t(f[17][79],_,[1,c],b);return g}catch(b){b=u(b);if(b===H){var
e=d(l[4],h8,c);return a(k[2],e)}throw b}}function
b6(n,b){function
m(h,ao){var
l=ao;for(;;){if(l){var
b=l[1];switch(b[0]){case
0:var
H=b[2],ap=l[2],aq=b[4],ar=b[3],as=x(h,b[1][1]),at=a(c[93],as),au=m(h,ap),av=a(f[17][1],H[3]),aw=a(c[93],av),ax=aJ(n,H[3],H[4]),ay=a(c[97][6],aq),az=[0,a(c[97][6],ar),ay,ax,aw,au,at],q=c[68],L=i(q),aA=j===L?q[1]:e===L?a(g[2],q):q;return a(A,[0,aA,az]);case
1:var
r=b[2],s=b[1],M=d(o[26],s[4],r),aB=d(o[8],M,r),aC=d(o[7],s[4],aB),aD=s[3],aE=function(a){return d(o[9],a,r)},N=d(o[43],aE,aD),aF=x(h,s[1]),aI=a(c[93],aF),aK=a(f[17][1],N),aL=a(c[93],aK),aM=aJ(n,N,M),aN=a(c[97][6],aC),aO=[0,a(c[97][6],r),aN,aM,aL,aI],t=c[69],O=i(t),aP=j===O?t[1]:e===O?a(g[2],t):t;return a(A,[0,aP,aO]);case
3:var
I=b[2],J=b[1],aQ=l[2],aR=J[3],aS=function(a){return d(o[9],a,I)},P=d(o[43],aS,aR),aT=d(o[26],J[4],I),aU=x(h,J[1]),aV=a(c[93],aU),aW=m(h,aQ),aX=a(f[17][1],P),aY=a(c[93],aX),aZ=aJ(n,P,aT),a0=[0,a(c[97][6],I),aZ,aY,aW,aV],u=c[70],Q=i(u),a1=j===Q?u[1]:e===Q?a(g[2],u):u;return a(A,[0,a1,a0]);case
4:var
R=b[3],S=R[2],T=R[1],U=b[2],V=U[2],W=U[1],a2=l[2],a3=b[1],a4=x(h,V[1]),a5=x(h,S[1]),a6=bW(W,V[3],T,S[3]),a7=m([0,[1,a3],h],a2),a8=a(c[93],a5),a9=a(c[97][6],T),a_=a(c[93],a4),a$=[0,a(c[97][6],W),a_,a9,a8,a6,a7],v=c[71],X=i(v),ba=j===X?v[1]:e===X?a(g[2],v):v;return a(A,[0,ba,a$]);case
5:var
p=b[1],Y=p[4],Z=p[3],_=p[2],bb=l[2],bc=p[5],be=p[1],bf=x(h,Z[1]),bg=x(h,_[1]),bh=aG(n,bc),bi=aH(n,_),bj=bd(n,[1,aH(n,Z),[2,[1,[4,bh],bi],[0,Y]]])[1],bk=m([0,[1,be[1]],h],bb),bl=a(c[93],bg),bm=a(c[93],bf),bn=[0,a(c[97][6],Y),bj,bm,bl,bk],w=c[72],$=i(w),bo=j===$?w[1]:e===$?a(g[2],w):w;return a(A,[0,bo,bn]);case
6:var
l=l[2];continue;case
9:var
aa=b[1],bp=b[2],bq=a(f[17][1],aa[3]),br=a(c[93],bq),bs=x(h,bp[1]),bt=a(c[93],bs),bu=x(h,aa[1]),bv=[0,br,a(c[93],bu),bt],y=c[73],ab=i(y),bw=j===ab?y[1]:e===ab?a(g[2],y):y;return a(A,[0,bw,bv]);case
10:var
K=b[2],ac=b[1];if(0===b[3]){var
bx=x(h,K[1]),by=a(c[93],bx),bz=x(h,ac[1]),bA=a(c[93],bz),bB=a(f[17][1],K[3]),bC=[0,a(c[93],bB),bA,by],z=c[78],ad=i(z),bD=j===ad?z[1]:e===ad?a(g[2],z):z;return a(A,[0,bD,bC])}var
bE=x(h,K[1]),bF=a(c[93],bE),bG=x(h,ac[1]),bH=[0,a(c[93],bG),bF],B=c[77],ae=i(B),bI=j===ae?B[1]:e===ae?a(g[2],B):B;return a(A,[0,bI,bH]);case
11:var
af=b[2],bJ=l[2],bK=b[3],bL=b[1],bM=x(h,af[1]),bN=x(h,bK),bO=m([0,[1,bL],h],bJ),bP=a(c[93],bN),bQ=a(c[93],bM),bR=a(f[17][1],af[3]),bS=[0,a(c[93],bR),bQ,bP,bO],C=c[74],ag=i(C),bT=j===ag?C[1]:e===ag?a(g[2],C):C;return a(A,[0,bT,bS]);case
12:var
bU=x(h,b[1]),bV=[0,a(c[93],bU)],D=c[66],ah=i(D),bX=j===ah?D[1]:e===ah?a(g[2],D):D;return a(A,[0,bX,bV]);case
13:var
bY=x(h,b[1]),bZ=[0,a(c[93],bY)],E=c[76],ai=i(E),b0=j===ai?E[1]:e===ai?a(g[2],E):E;return a(A,[0,b0,bZ]);case
14:var
b1=x(h,b[1]),b2=[0,a(c[93],b1)],F=c[67],aj=i(F),b3=j===aj?F[1]:e===aj?a(g[2],F):F;return a(A,[0,b3,b2]);case
15:var
ak=b[3],al=b[2],am=b[1],b4=ak[2],b5=ak[1],b6=al[2],b7=al[1],b8=x(h,am[1]),b9=m([0,[1,b7],h],b6),b_=m([0,[1,b5],h],b4),b$=a(c[93],b8),ca=a(f[17][1],am[3]),cb=[0,a(c[93],ca),b$,b9,b_],G=c[75],an=i(G),cc=j===an?G[1]:e===an?a(g[2],G):G;return a(A,[0,cc,cb]);case
16:return a(k[2],h9);default:var
l=l[2];continue}}return a(k[2],h_)}}return function(a){return m(b,a)}}function
bh(i,h,g,b){if(b){var
j=b[1],p=b[2];try{var
x=d(E[6],h[6],j),e=x}catch(b){b=u(b);if(b!==H)throw b;var
q=d(l[4],ic,j),e=a(k[2],q)}var
n=bf(e[5],g),o=n[2],r=n[1],s=e[6]?d(f[18],o,id):o,t=bh(i,h,[0,[1,e[8][1]],g],p),v=bg(s),w=[0,a(c[93],r),v,t];return m(c[84],w)}var
y=[0,a(b6(h,g),i)];return m(c[85],y)}function
aS(h,g,e){if(0===e[0]){var
i=e[1];return bh(i[3],h,g,i[2])}var
j=e[1],o=e[3],p=e[2];try{var
B=d(E[6],h[7],j),b=B}catch(c){c=u(c);if(c!==H)throw c;var
q=d(l[4],h$,j),b=a(k[2],q)}var
n=bf(b,g),r=n[2],s=n[1],t=d(f[18],b[2],ia),v=[0,[0,b[1],t]],w=d(f[18],b[2],ib),x=aS(h,[0,[0,[0,b[1],w]],g],o),y=aS(h,[0,v,g],p),z=bg(r),A=[0,a(c[93],s),z,y,x];return m(c[83],A)}function
b7(b,F,Q){var
G=[0,0];function
R(i){var
g=G[1];function
j(a){return a[8]}var
m=d(f[17][12],j,i),c=d(o[69],[0,a7,a9,an],m),e=ar(c),h=be(b,e);if(L[1]){d(l[2],ie,g);d(o[36],an,c);a(k[27],ig);var
n=function(a){return d(l[2],ih,a)};d(f[17][11],n,e);a(k[27],ii);d(f[17][11],aO,h);a(l[2],ij)}G[1]++;return[0,[0,g,e,c],h]}if(L[1])a(l[2],ik);var
n=aR(d(f[17][12],R,Q),0);if(L[1]){aQ(k[24],n);a(k[32],0)}var
H=aP(n);function
S(a){return bb(b,a)}var
x=d(f[17][12],S,H);function
T(a){return a[5][1]}var
W=d(f[17][12],T,x),X=a(f[17][95],W),p=[0,V,t(f[17][88],v[1][1],V,X)];function
Y(a){return t(f[17][bt],v[1][1],a,F)}var
y=d(f[17][12],Y,p),Z=aK(x),_=d(D,Z,P(t(f[17][bt],v[1][1],V,F))),h=b2(b,n);function
$(a){return a[1]}var
aa=d(f[17][12],$,h);function
ac(a){return a[2]}var
ae=d(f[17][12],ac,h);function
af(a){return[1,a[4]]}var
ag=d(f[17][12],af,h),ah=d(f[18],_,aa);function
I(d,c){if(c){var
e=c[1],f=c[2],g=a(ba(b),e);t(E[5],b[4],e,d);return[0,g,I(d+1|0,f)]}return 0}var
ai=I(0,ah),q=c[97][1],J=i(q),aj=j===J?q[1]:e===J?a(g[2],q):q,ak=d(c[94],aj,ai);function
al(d){var
a=d[3],e=a[1],f=z(b,a[2]),g=[0,z(b,e),f];return m(c[21],g)}var
am=d(f[17][12],al,h);if(y){var
A=y[1];if(typeof
A==="number")var
C=1;else
if(1===A[0])var
K=aI(b,A[1]),B=1,C=0;else
var
C=1;if(C)var
B=0}else
var
B=0;if(!B)var
K=aI(b,1);var
ao=a(f[17][4],y);function
ap(a){return aI(b,b5(H,a))}var
aq=d(f[17][12],ap,ao),as=a(c[95],b[2]),at=d(f[18],am,aq),r=c[20],M=i(r),au=j===M?r[1]:e===M?a(g[2],r):r,av=[0,K,as,ak,d(c[94],au,at)],aw=m(c[86],av);function
ax(b){function
d(a){if(a){if(1===a[1]){var
e=[0,d(a[2])];return m(c[11],e)}var
f=[0,d(a[2])];return m(c[10],f)}var
g=[0,b[4]],h=b[6]?c[12]:c[13];return m(h,g)}var
e=t(f[17][79],v[1][1],b[5][1],p),g=0===e?0:e+a(f[17][1],h)|0,i=d(b[5][2]),j=[0,a(c[93],g),i];return m(c[9],j)}var
ay=d(f[17][12],ax,x),u=c[8],N=i(u),az=j===N?u[1]:e===N?a(g[2],u):u,aA=d(c[94],az,ay),aB=a(f[17][4],p);function
aC(a){return[0,[0,a,0]]}var
aD=d(f[17][12],aC,aB),aE=aS(b,d(f[18],[0,[0,[0,V,0]],ag],aD),n),w=c[7],O=i(w),aF=j===O?w[1]:e===O?a(g[2],w):w,aG=a(ab[85],aF),aH=a(U[66][8],aG),aJ=a(U[66][8],ab[66]),aL=m(c[87],[0,aE,aA]),aM=a(ab[85],aL),aN=a(U[66][8],aM),aT=a(ab[52],aw),aU=a(U[66][8],aT),aV=a(f[17][4],p),aW=d(f[17][12],s[113],aV),aX=d(f[18],ae,aW),aY=a(ab[148],aX);return d(ad,d(ad,d(ad,d(ad,d(ad,a(U[66][8],aY),aU),aN),bI),aJ),aH)}var
bi=[0,o,L,bI,gW,ad,A,bJ,V,bK,bL,bM,gY,bN,a7,bO,a9,bP,an,a_,bQ,aG,a$,ba,bR,g7,bS,bb,B,F,C,bT,aH,m,bU,bV,z,aI,aJ,D,M,aK,P,ao,aL,hE,bW,ae,bc,ap,af,aM,Q,bd,bX,bY,W,aN,aq,X,ag,bZ,R,Y,b0,aO,b1,ar,as,b2,be,b3,b4,aP,b5,aQ,aR,bf,bg,x,b6,aS,bh,b7,function(b){a(I[11],il);bO(0);bP(0);try{var
c=bK(0),d=bZ(c,b),e=b0(d);if(L[1])b1(e);var
f=a(b7(c,d,e),b);return f}catch(b){b=u(b);if(b===o[28])return a(J[6],im);throw b}}];aV(302,bi,"Romega_plugin.Refl_omega");a(bj[12],is);function
at(b){var
c=d(bk[13],v[1][5],it),e=a(v[5][4],c),f=a(v[6][4],b),g=d(v[13][2],[0,e],f),h=a(aT[6],g);return a(b8[17],h)}function
aU(b){var
c=d(f[17][96],iq[29],b);function
e(b){if(n(b,iu)){if(n(b,iv)){if(n(b,iw)){if(n(b,ix)){var
c=d(k[16],iy,b);return a(J[6],c)}return at(iz)}return at(iA)}return at(iB)}return at(iC)}var
g=d(bk[13],e,c),h=a(U[66][1],bi[84]),i=d(ac[70][3],ab[28],h),j=a(ac[70][20],g),l=a(U[55],j),m=a(ac[70][28],l);return d(ac[70][3],m,i)}function
iD(d){var
b=[28,[0,0,[31,b9[4],[0,[0,Z,iE],0],0]]],c=a(v[1][5],iF);return bn(aT[4],1,0,c,b)}var
iG=[0,function(b,a){return aU(0)}];t(aT[9],0,[0,Z,iH],iG);d(bj[19],iD,Z);var
iI=0,iL=[0,function(b){return b?a(k[2],iJ):function(a){return aU(iK)}},iI],iN=[0,function(b){if(b)if(!b[2]){var
c=b[1],e=a(b_[17],b$[4]),f=a(b_[6],e),g=d(b8[2][7],f,c);return function(a){return aU(d(bk[13],v[1][7],g))}}return a(k[2],iM)},iL],iO=a(ir[12],iN);t(aT[9],0,[0,Z,iP],iO);function
iQ(f){var
e=a(v[1][6],iS),b=b$[4],c=0;if(0===b[0])return d(ip[4],[0,Z,iW],[0,[0,iV,[0,iU,[0,[1,b9[4],[0,[5,[0,b[1]]]],e],c]]],iR]);throw[0,io,iT]}d(bj[19],iQ,Z);var
ca=[0,Z,at,aU];aV(314,ca,"Romega_plugin.G_romega");aV(315,[0,c,bi,ca],"Romega_plugin");return});
