(function(hr){"use strict";var
cl="xO",cq=" - ",cr="not",bm=123,cH=131,q=250,cp="Z.succ",cD="Z.pred",bo="Z.opp",ct="xH",aS="Zpos",cC="REIFED PROBLEM\n\n",d=246,co="FF",bj="Z.sub",L=119,cx="True",ao="Z",aR="Extension: cannot occur",X="romega",aU="plugins/romega/refl_omega.ml",bl=120,cB="\n====================================\n",bn="N",cG="False",cw="with",bk="Z.mul",cF="Prop",cA="romega'",aQ="",cI=135,cs="or",cz="TT",aT="Omega",ck="=SYSTEM===================================\n",aP="Zneg",bi="Z.add",cn="positive",aO="Z0",cy="  Depends on:",cv=126,cE="nat",cu="and",cm="\n------------------------------------\n",cj="xI",W="Coq",w=hr.jsoo_runtime,ci=w.caml_int_compare,a=w.caml_new_string,p=w.caml_obj_tag,aN=w.caml_register_global,h=w.caml_string_notequal,ch=w.caml_trampoline,cg=w.caml_trampoline_return,u=w.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):w.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):w.caml_call_gen(a,[b,c])}function
l(a,b,c,d){return a.length==3?a(b,c,d):w.caml_call_gen(a,[b,c,d])}function
bh(a,b,c,d,e){return a.length==4?a(b,c,d,e):w.caml_call_gen(a,[b,c,d,e])}function
hq(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):w.caml_call_gen(a,[b,c,d,e,f])}function
ac(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):w.caml_call_gen(a,[b,c,d,e,f,g])}function
cf(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):w.caml_call_gen(a,[b,c,d,e,f,g,h])}var
i=w.caml_get_global_data(),Q=a("romega_plugin"),o=i.CamlinternalLazy,t=i.Term,g=i.Bigint,R=i.Option,F=i.EConstr,aY=i.Tacmach,s=i.Names,aX=i.Univ,E=i.Coqlib,bt=i.Global,ae=i.Universes,aW=i.Nametab,f=i.Util,j=i.Pervasives,k=i.Printf,x=i.Int,ag=i.Assert_failure,M=i.Not_found,r=i.Pp,b$=i.CErrors,ca=i.Proofview,U=i.Tactics,bJ=i.Invalid_argument,Z=i.Tacticals,cc=i.Stdarg,V=i.Ltac_plugin,be=i.List,cd=i.Genarg,bd=i.Mltop,cb=i.Array,bp=[0,a(W),[0,a(X),[0,a("ReflOmegaCore"),0]]],fj=a(bi),fk=a(bk),fl=a(bo),fm=a(cD),fn=a(bj),fo=a(cp),fp=a(aO),fq=a(aP),fr=a(aS),fc=a("Z.ge"),fd=a("Z.gt"),fe=a("Z.le"),ff=a("Z.lt"),fg=a("Zne"),fh=a("eq"),fi=a(ao),e3=a(bi),e4=a(bk),e5=a(bo),e6=a(cD),e7=a(bj),e8=a(cp),e9=a(aO),e_=a(aP),e$=a(aS),eZ=a(aO),e0=a(aP),e1=a(aS),eW=a(ct),eX=a(cj),eY=a(cl),eU=a(bj),eS=a(bo),eQ=a(bk),eO=a(bi),eM=a(ao),eK=a("Npos"),eJ=a("N0"),eI=a(aP),eH=a(aS),eG=a(aO),eF=a(cj),eE=a(cl),eD=a(ct),ex=a(cG),ey=a(cx),ez=a(cu),eA=a("iff"),eB=a(cr),eC=a(cs),er=a("nil"),eq=a("cons"),en=[0,a("Init"),[0,a("Datatypes"),0]],eo=a(aQ),em=a("O"),el=a("S"),ej=a("do_omega"),eh=a("interp_goal_concl"),ef=a("E_SOLVE"),ed=a("E_EXTRACT"),eb=a("E_SPLIT"),d$=a("D_right"),d9=a("D_left"),d7=a("direction"),d5=a("O_SPLIT_INEQ"),d3=a("O_MERGE_EQ"),d1=a("O_SUM"),dZ=a("O_NOT_EXACT_DIVIDE"),dX=a("O_DIVIDE"),dV=a("O_BAD_CONSTANT"),dT=a("Tprop"),dR=a("Timp"),dP=a("Tand"),dN=a("Tor"),dL=a("Tnot"),dJ=a("FalseTerm"),dH=a("TrueTerm"),dF=a("NeqTerm"),dD=a("GtTerm"),dB=a("LtTerm"),dz=a("GeqTerm"),dx=a("LeqTerm"),dv=a("EqTerm"),dt=a("proposition"),dr=a("Tvar"),dp=a("Tminus"),dm=a("Topp"),dk=a("Tmult"),di=a("Tplus"),dg=a("Tint"),de=a("I"),dc=a(cG),da=a(cx),c_=a(cs),c8=a(cr),c6=a(cu),c4=a("eq_refl"),c3=a(aT),c2=a(aT),c1=a(aT),c0=a(aT),cK=a("."),cL=a(aQ),cM=a(aQ),cJ=[0,a(ao),[0,a(bn),[0,a("Pos"),0]]],cN=a("Const_omega.DestConstApp"),cO=[0,a(W),[0,a("Logic"),[0,a("Decidable"),0]]],cP=[0,a("ZOmega"),0],cS=[0,[0,a(W),[0,a("Lists"),[0,a("List"),0]]],0],cY=[0,[0,a(W),[0,a("Numbers"),[0,a("BinNums"),0]]],0],cZ=[0,[0,a(W),[0,a("ZArith"),[0,a("BinInt"),0]]],0],fO=[0,[2,0,0],a("%s")],fP=[0,[12,40,[15,[11,a(" + "),[15,[12,41,0]]]]],a("(%a + %a)")],fQ=[0,[12,40,[15,[11,a(" * "),[15,[12,41,0]]]]],a("(%a * %a)")],fR=[0,[12,40,[15,[11,a(cq),[15,[12,41,0]]]]],a("(%a - %a)")],fS=[0,[11,a("~ "),[15,0]],a("~ %a")],fT=[0,[12,86,[4,0,[0,2,2],0,0]],a("V%02d")],f0=[0,[11,a(cz),0],a(cz)],f1=[0,[11,a(co),0],a(co)],f2=[0,[15,[12,32,[2,0,[12,32,[15,0]]]]],a("%a %s %a")],f3=[0,[11,a("not("),[15,[12,41,0]]],a("not(%a)")],f4=[0,[12,40,[15,[11,a(" or "),[15,[12,41,0]]]]],a("(%a or %a)")],f5=[0,[12,40,[15,[11,a(" and "),[15,[12,41,0]]]]],a("(%a and %a)")],f6=[0,[12,40,[15,[11,a(" => "),[15,[12,41,0]]]]],a("(%a => %a)")],f7=[0,[11,a(cF),0],a(cF)],ga=[0,a(aU),439,15],gg=[0,0,0],gx=a(" "),gy=[0,[4,0,0,0,[12,91,[2,0,[12,93,0]]]],a("%d[%s]")],gz=[0,[12,83,[4,0,0,0,[12,40,[15,[12,44,[15,[12,41,0]]]]]]],a("S%d(%a,%a)")],gA=[0,[4,0,0,0,[11,a(cq),[4,0,0,0,[12,10,0]]]],a("%d - %d\n")],gE=a("not_treated"),gD=a("bad history"),gF=[0,[11,a("Cannot find constructor "),[4,0,0,0,0]],a("Cannot find constructor %d")],gG=[0,0,0],gH=[0,1,0],gI=[0,[11,a("Cannot find equation "),[4,0,0,0,0]],a("Cannot find equation %d")],gP=[0,a(W),[0,a(X),[0,a("ROmega"),0]]],gQ=a("ROmega can't solve this system"),gO=[0,[11,a(cB),0],a(cB)],gL=[0,[12,32,[4,0,0,0,0]],a(" %d")],gJ=[0,[11,a("SYSTEME "),[4,0,0,0,[12,10,0]]],a("SYSTEME %d\n")],gK=a("\n  Depend :"),gM=a("\n  Split points :"),gN=[0,[11,a(cm),0],a(cm)],gC=[0,[11,a("get_hyp "),[4,0,0,0,0]],a("get_hyp %d")],gB=a("find_path"),gw=a("select_smaller"),gu=[0,0,0,0,0],gt=[0,[11,a(ck),0],a(ck)],gl=a("L"),gm=a("R"),gn=a("M"),gk=[0,[11,a(cy),0],a(cy)],go=a(aQ),gp=[0,[11,a("\n  Path: "),[2,0,0]],a("\n  Path: %s")],gq=a("yes"),gs=a("no"),gr=[0,[11,a("\n  Origin: "),[2,0,[11,a(" (negated : "),[2,0,[11,a(")\n\n"),0]]]]],a("\n  Origin: %s (negated : %s)\n\n")],gj=[0,[11,a("  E"),[4,0,0,0,[11,a(" : "),[15,[12,32,[2,0,[11,a(" 0\n"),0]]]]]]],a("  E%d : %a %s 0\n")],gh=[0,[11,a(" L"),[4,0,0,0,0]],a(" L%d")],gi=[0,[11,a(" R"),[4,0,0,0,0]],a(" R%d")],gf=[0,2,0],ge=[0,[11,a("  "),[2,0,[11,a(": "),[15,[12,10,0]]]]],a("  %s: %a\n")],gc=[0,[11,a(cC),0],a(cC)],gd=[0,[11,a("  CONCL: "),[15,[12,10,0]]],a("  CONCL: %a\n")],f$=[0,a(aU),348,9],f_=[0,a(aU),323,9],f9=[0,[4,0,0,0,[11,a(" -> "),[4,0,0,0,[12,10,0]]]],a("%d -> %d\n")],f8=[0,[11,a("Atome "),[4,0,0,0,[11,a(" non trouv\xc3\xa9\n"),0]]],a("Atome %d non trouv\xc3\xa9\n")],fU=a("="),fV=a("<="),fW=a(">="),fX=a(">"),fY=a("<"),fZ=a("!="),fN=[0,[11,a("Omega Equation "),[4,0,0,0,[11,a(" non trouv\xc3\xa9e\n"),0]]],a("Omega Equation %d non trouv\xc3\xa9e\n")],fM=a("get_prop"),fK=[0,a(aU),234,2],fJ=a("get_reified_atom"),fI=[0,[11,a("OV"),[4,0,0,0,0]],a("OV%d")],fB=[0,[12,40,[0,[4,0,[0,2,2],0,[12,41,0]]]],a("(%c%02d)")],fD=a(" := "),fE=a("  ===============================\n\n"),fF=a("ENVIRONMENT OF PROPOSITIONS :"),fG=a("ENVIRONMENT OF TERMS :"),fz=a("__goal__"),hl=[0,a("plugins/romega/g_romega.ml4"),1,0],hj=[0,[0,[0,a(X)],[0,[0,a(cw)],[0,[0,a("*")],0]]],0],hk=a("$l"),hn=[0,a(cw)],ho=[0,a(X)],hp=a(cA),he=a(aR),hc=[0,a(cE),[0,a(cn),[0,a(bn),[0,a(ao),0]]]],hb=a(aR),g_=[0,[0,[0,a(X)],0],[0,[0,[0,a("unsafe_romega")],0],0]],g$=a(X),g5=a(aR),g3=a(aR),gT=a(bn),gU=a(ao),gV=a(cE),gW=a(cn),gY=a("zify_positive"),gZ=a("zify_nat"),g0=a("zify_op"),g1=a("zify_N"),gX=a("No ROmega knowledge base for type "),gR=[0,a("PreOmega"),[0,a("omega"),[0,a(W),0]]],g8=a(X),hh=a(cA),fa=i.Tacred,ep=i.Globnames,gv=i.Failure,gb=i.Logic,fC=i.Printer,fH=i.Feedback,fs=i.Omega_plugin,fx=i.Hashtbl,hm=i.Loc,gS=i.String;function
aV(a){var
h=b(aW[40],a),d=b(s[5][5],h);if(d)var
e=b(s[1][8],d[1]),i=c(f[15][50][2],e,cJ)?c(j[16],e,cK):cL,g=i;else
var
g=cM;var
k=b(aW[41],a),l=b(s[1][8],k);return c(j[16],g,l)}function
ad(e){var
d=b(t[38],e),c=d[2],a=b(t[cI],d[1]);switch(a[0]){case
1:if(!c)return[0,b(s[1][8],a[1])];break;case
6:if(!a[1])if(!c)return[2,a[2],a[3]];break;case
10:return[1,aV([1,a[1][1]]),c];case
11:return[1,aV([2,a[1][1]]),c];case
12:return[1,aV([3,a[1][1]]),c]}return 0}var
ap=[248,cN,w.caml_fresh_oo_id(0)];function
bq(e){var
d=b(t[38],e),f=d[2],a=b(t[cI],d[1]);switch(a[0]){case
10:var
c=[1,a[1][1]];break;case
11:var
c=[2,a[1][1]];break;case
12:var
c=[3,a[1][1]];break;default:throw ap}return[0,b(aW[41],c),f]}var
cQ=[0,c(j[25],bp,cP),0],cR=c(j[25],[0,bp,0],cQ),cT=c(j[25],cS,cR),cU=c(j[25],E[6],cT),cV=c(j[25],E[5],cU),cW=c(j[25],[0,cO,0],cV),cX=c(j[25],E[7],cW);function
H(a){var
c=l(E[4],c0,E[7],a);return b(ae[45],c)}function
m(a){var
c=l(E[4],c1,cX,a);return b(ae[45],c)}function
aq(a){var
c=l(E[4],c2,cZ,a);return b(ae[45],c)}function
I(a){var
c=l(E[4],c3,cY,a);return b(ae[45],c)}var
c5=[d,function(a){return H(c4)}],c7=[d,function(a){return H(c6)}],c9=[d,function(a){return H(c8)}],c$=[d,function(a){return H(c_)}],db=[d,function(a){return H(da)}],dd=[d,function(a){return H(dc)}],df=[d,function(a){return H(de)}],dh=[d,function(a){return m(dg)}],dj=[d,function(a){return m(di)}],dl=[d,function(a){return m(dk)}],dn=[d,function(a){return m(dm)}],dq=[d,function(a){return m(dp)}],ds=[d,function(a){return m(dr)}],du=[d,function(a){return m(dt)}],dw=[d,function(a){return m(dv)}],dy=[d,function(a){return m(dx)}],dA=[d,function(a){return m(dz)}],dC=[d,function(a){return m(dB)}],dE=[d,function(a){return m(dD)}],dG=[d,function(a){return m(dF)}],dI=[d,function(a){return m(dH)}],dK=[d,function(a){return m(dJ)}],dM=[d,function(a){return m(dL)}],dO=[d,function(a){return m(dN)}],dQ=[d,function(a){return m(dP)}],dS=[d,function(a){return m(dR)}],dU=[d,function(a){return m(dT)}],dW=[d,function(a){return m(dV)}],dY=[d,function(a){return m(dX)}],d0=[d,function(a){return m(dZ)}],d2=[d,function(a){return m(d1)}],d4=[d,function(a){return m(d3)}],d6=[d,function(a){return m(d5)}],d8=[d,function(a){return m(d7)}],d_=[d,function(a){return m(d9)}],ea=[d,function(a){return m(d$)}],ec=[d,function(a){return m(eb)}],ee=[d,function(a){return m(ed)}],eg=[d,function(a){return m(ef)}],ei=[d,function(a){return m(eh)}],ek=[d,function(a){return m(ej)}],ar=[d,function(a){return H(el)}],as=[d,function(a){return H(em)}];function
br(a){if(0===a){var
c=p(as);return q===c?as[1]:d===c?b(o[2],as):as}var
e=p(ar),f=[0,br(a-1|0)],g=q===e?ar[1]:d===e?b(o[2],ar):ar;return b(t[L],[0,g,f])}function
bs(c){var
a=l(E[2],eo,en,c),d=b(bt[45],a)?function(a){return b(aX[29][3],[0,a])}:function(a){return aX[29][1]};return function(c){var
e=d(c),f=[0,b(ep[10],a),e];return b(t[cv],f)}}function
bu(d,c,a){function
e(a){if(a){var
h=a[1],i=[0,h,e(a[2])],f=[0,b(bs(eq),d),[0,c]],j=[0,b(t[L],f),i];return b(t[L],j)}var
g=[0,b(bs(er),d),[0,c]];return b(t[L],g)}return e(a)}var
es=b(bt[54],0),et=b(ae[7],es);function
eu(a){return bu(et,t[112],a)}var
ev=aX[1][1];function
ew(a,b){return bu(ev,a,b)}var
at=[d,function(a){return I(eD)}],au=[d,function(a){return I(eE)}],av=[d,function(a){return I(eF)}],aw=[d,function(a){return I(eG)}],ax=[d,function(a){return I(eH)}],ay=[d,function(a){return I(eI)}],az=[d,function(a){return I(eJ)}],aA=[d,function(a){return I(eK)}];function
aB(a){if(c(g[17],a,g[6])){var
e=p(at);return q===e?at[1]:d===e?b(o[2],at):at}var
f=c(g[15],a,g[7]),k=f[2],l=[0,aB(f[1])];if(c(g[17],k,g[5]))var
h=p(au),m=q===h?au[1]:d===h?b(o[2],au):au,i=m;else
var
j=p(av),n=q===j?av[1]:d===j?b(o[2],av):av,i=n;return b(t[L],[0,i,l])}function
eL(a){if(0===a){var
c=p(az);return q===c?az[1]:d===c?b(o[2],az):az}var
e=p(aA),f=[0,aB(b(g[3],a))],h=q===e?aA[1]:d===e?b(o[2],aA):aA;return b(t[L],[0,h,f])}var
eN=[d,function(a){return I(eM)}],eP=[d,function(a){return aq(eO)}],eR=[d,function(a){return aq(eQ)}],eT=[d,function(a){return aq(eS)}],eV=[d,function(a){return aq(eU)}];function
bv(a){function
d(i){var
f=bq(i),a=f[2],e=b(s[1][8],f[1]);if(h(e,eW)){if(h(e,eX)){if(!h(e,eY))if(a)if(!a[2]){var
j=d(a[1]);return c(g[14],g[7],j)}}else
if(a)if(!a[2]){var
k=d(a[1]),l=c(g[14],g[7],k);return c(g[12],g[6],l)}}else
if(!a)return g[6];throw ap}try{var
e=[0,d(a)];return e}catch(a){a=u(a);if(a===ap)return 0;throw a}}function
bw(j){try{var
i=bq(j),d=i[2],f=b(s[1][8],i[1]);if(h(f,eZ))if(h(f,e0))if(h(f,e1))var
a=0;else
if(d)if(d[2])var
a=0;else
var
e=bv(d[1]),a=1;else
var
a=0;else
if(d)if(d[2])var
a=0;else
var
k=bv(d[1]),e=c(R[15],g[22],k),a=1;else
var
a=0;else
if(d)var
a=0;else
var
e=[0,g[5]],a=1;if(!a)var
e=0;return e}catch(a){a=u(a);if(a===ap)return 0;throw a}}function
bx(a){if(c(g[17],a,g[5])){var
e=p(aw);return q===e?aw[1]:d===e?b(o[2],aw):aw}if(b(g[18],a)){var
f=p(ax),i=[0,aB(a)],j=q===f?ax[1]:d===f?b(o[2],ax):ax;return b(t[L],[0,j,i])}var
h=p(ay),k=[0,aB(b(g[22],a))],l=q===h?ay[1]:d===h?b(o[2],ay):ay;return b(t[L],[0,l,k])}function
e2(o){var
a=ad(o);if(typeof
a!=="number"&&1===a[0]){var
c=a[1];if(h(c,e3))if(h(c,e4))if(h(c,e5))if(h(c,e6))if(h(c,e7))if(h(c,e8)){var
r=h(c,e9)?h(c,e_)?h(c,e$)?1:0:0:0;if(!r){var
p=bw(o);return p?[5,p[1]]:0}}else{var
d=a[2];if(d)if(!d[2])return[4,d[1]]}else{var
e=a[2];if(e){var
f=e[2];if(f)if(!f[2])return[2,e[1],f[1]]}}else{var
i=a[2];if(i)if(!i[2]){var
q=i[1];return[0,q,bx(b(g[22],g[6]))]}}else{var
j=a[2];if(j)if(!j[2])return[3,j[1]]}else{var
k=a[2];if(k){var
l=k[2];if(l)if(!l[2])return[1,k[1],l[1]]}}else{var
m=a[2];if(m){var
n=m[2];if(n)if(!n[2])return[0,m[1],n[1]]}}}return 0}function
fb(G,C){var
c=ad(C);if(typeof
c!=="number"&&1===c[0]){var
e=c[1];if(h(e,fc))if(h(e,fd))if(h(e,fe))if(h(e,ff))if(h(e,fg)){if(!h(e,fh)){var
p=c[2];if(p){var
q=p[2];if(q){var
r=q[2];if(r)if(!r[2]){var
H=r[1],I=q[1],D=b(F[8],p[1]),E=l(aY[48][1],fa[9],G,D),f=ad(b(F[bm][1],E));if(typeof
f!=="number"&&1===f[0])if(!h(f[1],fi))if(!f[2])return[0,I,H];return 2}}}}}else{var
s=c[2];if(s){var
t=s[2];if(t)if(!t[2])return[1,s[1],t[1]]}}else{var
u=c[2];if(u){var
v=u[2];if(v)if(!v[2])return[2,u[1],v[1]]}}else{var
w=c[2];if(w){var
x=w[2];if(x)if(!x[2])return[3,w[1],x[1]]}}else{var
y=c[2];if(y){var
z=y[2];if(z)if(!z[2])return[4,y[1],z[1]]}}else{var
A=c[2];if(A){var
B=A[2];if(B)if(!B[2])return[5,A[1],B[1]]}}}var
a=ad(C);if(typeof
a!=="number")switch(a[0]){case
1:var
d=a[1];if(h(d,ex)){if(h(d,ey))if(h(d,ez))if(h(d,eA))if(h(d,eB)){if(!h(d,eC)){var
g=a[2];if(g){var
i=g[2];if(i)if(!i[2])return[7,g[1],i[1]]}}}else{var
j=a[2];if(j)if(!j[2])return[6,j[1]]}else{var
k=a[2];if(k){var
m=k[2];if(m)if(!m[2])return[10,k[1],m[1]]}}else{var
n=a[2];if(n){var
o=n[2];if(o)if(!o[2])return[8,n[1],o[1]]}}else
if(!a[2])return 0}else
if(!a[2])return 1;break;case
2:return[9,a[1],a[2]]}return 2}function
G(p){var
a=ad(p);if(typeof
a!=="number"&&1===a[0]){var
b=a[1];if(h(b,fj))if(h(b,fk))if(h(b,fl))if(h(b,fm))if(h(b,fn))if(h(b,fo)){var
C=h(b,fp)?h(b,fq)?h(b,fr)?1:0:0:0;if(!C)return bw(p)}else{var
d=a[2];if(d)if(!d[2]){var
q=G(d[1]);return c(R[15],g[9],q)}}else{var
e=a[2];if(e){var
f=e[2];if(f)if(!f[2]){var
r=e[1],s=G(f[1]),t=G(r);return l(R[26],g[13],t,s)}}}else{var
i=a[2];if(i)if(!i[2]){var
u=G(i[1]);return c(R[15],g[10],u)}}else{var
j=a[2];if(j)if(!j[2]){var
v=G(j[1]);return c(R[15],g[22],v)}}else{var
k=a[2];if(k){var
m=k[2];if(m)if(!m[2]){var
w=k[1],x=G(m[1]),y=G(w);return l(R[26],g[14],y,x)}}}else{var
n=a[2];if(n){var
o=n[2];if(o)if(!o[2]){var
z=n[1],A=G(o[1]),B=G(z);return l(R[26],g[12],B,A)}}}}return 0}var
e=[0,c5,c7,c9,c$,db,dd,df,dh,dj,dl,dn,dq,ds,du,dw,dy,dA,dC,dE,dG,dI,dK,dM,dO,dQ,dS,dU,dW,dY,d0,d2,d4,d6,d8,d_,ea,ec,ee,eg,ei,ek,br,eL,ew,eu,[0,eN,eP,eR,eT,eV,bx,e2,fb,G]];aN(232,e,"Romega_plugin.Const_omega");var
v=b(fs[1][2],[0,g[17],g[16],g[12],g[13],g[14],g[15],g[22],g[5],g[6],g[2]]),ft=0,fu=0,fv=x[1],fw=[0,function(b,a){return b===a?1:0},fv],z=b(fx[19],fw),Y=[0,0],by=Z[66][2];function
fy(a){b(j[32],a);b(j[35],0);return b(j[51],j[27])}var
_=Z[66][3],C=t[L];function
bz(c,a){switch(c){case
0:var
b=0===a?1:0;break;case
1:var
b=1===a?1:0;break;default:var
b=2<=a?1:0}return b?1:0}function
bA(l,k){var
b=l,a=k;for(;;){switch(b[0]){case
0:if(0===a[0])return c(g[17],b[1],a[1]);var
d=0;break;case
1:if(1===a[0])var
i=a[2],h=a[1],f=b[2],e=b[1],d=1;else
var
d=0;break;case
2:if(2===a[0])var
i=a[2],h=a[1],f=b[2],e=b[1],d=1;else
var
d=0;break;case
3:if(3===a[0])var
i=a[2],h=a[1],f=b[2],e=b[1],d=1;else
var
d=0;break;case
4:if(4===a[0]){var
b=b[1],a=a[1];continue}var
d=0;break;default:if(5===a[0])return b[1]===a[1]?1:0;var
d=0}if(d){var
j=bA(e,h);if(j){var
b=f,a=i;continue}return j}return 0}}function
aC(c,a){if(0===c[0]){var
f=c[1];if(0===a[0])var
e=a[1],d=f,b=1;else
var
b=0}else{var
g=c[1];if(0===a[0])var
b=0;else
var
e=a[1],d=g,b=1}return b?d===e?1:0:0}var
af=b(s[1][6],fz);function
bB(d){var
a=b(z[1],7),c=b(z[1],7);return[0,0,0,b(z[1],7),0,c,a]}function
bC(a){a[4]=a[4]+1|0;return a[4]}function
bD(a){return 0===a[0]?[1,a[1]]:[0,a[1]]}function
fA(a){return a[1]}function
bE(d){function
a(f,e,d){if(d){var
g=d[2],h=d[1],i=l(k[4],fB,f,e),j=a(f,e+1|0,g),m=b(r[5],0),n=b(fC[5],h),o=b(r[3],fD),p=b(r[3],i),q=b(r[13],0),s=c(r[12],q,p),t=c(r[12],s,o),u=c(r[12],t,n),v=c(r[12],u,m);return c(r[12],v,j)}return b(r[3],fE)}var
e=a(80,0,d[2]),f=b(r[5],0),g=b(r[3],fF),h=c(r[12],g,f),i=c(r[12],h,e),j=a(86,0,d[1]),m=b(r[5],0),n=b(r[3],fG),o=c(r[12],n,m),p=c(r[12],o,j),q=b(r[5],0),s=c(r[12],i,q),t=c(r[12],s,p);return c(fH[10],0,t)}var
aZ=[0,-1];function
bF(a){aZ[1]=-1;return 0}function
a0(a){aZ[1]++;return aZ[1]}var
aD=[0,-1];function
bG(a){aD[1]=a;return 0}function
bH(a){aD[1]=-1;return 0}function
bI(a){aD[1]++;return aD[1]}function
aE(a){return c(k[4],fI,a)}function
a1(e,a){try{var
d=l(f[17][86],t[cH],e,a[1]);return d}catch(d){d=u(d);if(d===M){var
g=b(f[17][1],a[1]);a[1]=c(f[18],a[1],[0,e,0]);return g}throw d}}function
a2(a){try{var
c=b(f[17][7],a[1]);return c}catch(a){a=u(a);if(a[1]===bJ)return b(j[2],fJ);throw a}}function
bK(e,d,a){if(e===b(f[17][1],a[1])){a[1]=c(f[18],a[1],[0,d,0]);return 0}throw[0,ag,fK]}function
bL(a,e){try{var
d=l(f[17][86],t[cH],e,a[2]);return d}catch(d){d=u(d);if(d===M){var
g=b(f[17][1],a[2]);a[2]=c(f[18],a[2],[0,e,0]);return g}throw d}}function
fL(d,a){try{var
e=c(f[17][7],d,a);return e}catch(a){a=u(a);if(a[1]===bJ)return b(j[2],fM);throw a}}function
bM(b,a){var
d=a[7][1];return c(z[11],b[5],d)?0:l(z[5],b[5],d,a)}function
a3(a,b){try{var
d=c(z[7],a[5],b);return d}catch(a){a=u(a);if(a===M){c(k[2],fN,b);throw a}throw a}}function
D(c,a){switch(a[0]){case
0:var
d=b(g[2],a[1]);return l(k[1],c,fO,d);case
1:return ac(k[1],c,fP,D,a[1],D,a[2]);case
2:return ac(k[1],c,fQ,D,a[1],D,a[2]);case
3:return ac(k[1],c,fR,D,a[1],D,a[2]);case
4:return bh(k[1],c,fS,D,a[1]);default:return l(k[1],c,fT,a[1])}}function
bN(a){switch(a){case
0:return fU;case
1:return fV;case
2:return fW;case
3:return fX;case
4:return fY;default:return fZ}}function
A(b,a){if(typeof
a==="number")return 0===a?c(k[1],b,f0):c(k[1],b,f1);else
switch(a[0]){case
0:var
d=a[2],e=d[3],f=d[2],g=bN(d[1]);return cf(k[1],b,f2,D,f,g,D,e);case
1:return bh(k[1],b,f3,A,a[1]);case
2:return ac(k[1],b,f4,A,a[2],A,a[3]);case
3:return ac(k[1],b,f5,A,a[2],A,a[3]);case
4:return ac(k[1],b,f6,A,a[2],A,a[3]);default:return c(k[1],b,f7)}}function
bO(b){function
c(a){if(a){var
d=a[1],e=d[2],f=d[1];return[1,[2,[5,e],[0,f]],c(a[2])]}return[0,b[4]]}return c(b[3])}function
n(a,e){var
c=p(a),f=q===c?a[1]:d===c?b(o[2],a):a;return b(C,[0,f,e])}function
bP(d,a){function
c(a){switch(a[0]){case
0:return b(e[46][6],a[1]);case
1:var
f=a[1],g=c(a[2]),h=[0,c(f),g];return n(e[46][2],h);case
2:var
i=a[1],j=c(a[2]),k=[0,c(i),j];return n(e[46][3],k);case
3:var
l=a[1],m=c(a[2]),o=[0,c(l),m];return n(e[46][5],o);case
4:var
p=[0,c(a[1])];return n(e[46][4],p);default:var
q=a[1];return b(a2(d),q)}}return c(a)}function
bQ(d,b){try{var
a=c(z[7],d[3],b);return a}catch(a){a=u(a);if(a===M){c(k[2],f8,b);var
e=d[3],f=function(b,a){return l(k[2],f9,b,a)};c(z[12],f,e);throw M}throw a}}function
bR(a){switch(a[0]){case
1:var
b=e[9];return function(a){return n(b,a)};case
2:var
c=e[10];return function(a){return n(c,a)};case
3:var
d=e[12];return function(a){return n(d,a)};default:throw[0,ag,f_]}}function
aF(c,a){switch(a[0]){case
0:var
d=[0,b(e[46][6],a[1])];return n(e[8],d);case
4:var
i=[0,aF(c,a[1])];return n(e[11],i);case
5:var
j=bQ(c,a[1]),k=[0,b(e[43],j)];return n(e[13],k);default:var
f=a[1],g=aF(c,a[2]),h=[0,aF(c,f),g];return b(bR(a),h)}}function
ah(a,b){try{var
c=aF(a,b);return c}catch(a){a=u(a);D(j[28],b);throw a}}function
bS(a){switch(a){case
0:var
b=e[15];return function(a){return n(b,a)};case
1:var
c=e[16];return function(a){return n(c,a)};case
2:var
d=e[17];return function(a){return n(d,a)};case
3:var
f=e[19];return function(a){return n(f,a)};case
4:var
g=e[18];return function(a){return n(g,a)};default:var
h=e[20];return function(a){return n(h,a)}}}function
bT(a){if(typeof
a!=="number")switch(a[0]){case
2:var
b=e[24];return function(a){return n(b,a)};case
3:var
c=e[25];return function(a){return n(c,a)};case
4:var
d=e[26];return function(a){return n(d,a)}}throw[0,ag,f$]}function
ai(c,a){if(typeof
a==="number"){if(0===a){var
f=e[21],i=p(f);return q===i?f[1]:d===i?b(o[2],f):f}var
g=e[22],j=p(g);return q===j?g[1]:d===j?b(o[2],g):g}else
switch(a[0]){case
0:var
h=a[2],k=h[2],l=h[1],m=ah(c,h[3]),r=[0,ah(c,k),m];return b(bS(l),r);case
1:var
s=[0,ai(c,a[1])];return n(e[23],s);case
5:var
w=bL(c,a[1]),x=[0,b(e[42],w)];return n(e[27],x);default:var
t=a[2],u=ai(c,a[3]),v=[0,ai(c,t),u];return b(bT(a),v)}}function
a4(a,b){try{var
c=ai(a,b);return c}catch(a){a=u(a);A(j[28],b);throw a}}function
bU(b,a){var
c=a[1],d=ah(b,a[2]),f=[0,ah(b,c),d];return n(e[15],f)}var
B=x[2][7];function
J(d){var
a=d;for(;;)switch(a[0]){case
0:return x[2][1];case
1:var
e=a[1],f=J(a[2]);return c(B,J(e),f);case
2:var
g=a[1],h=J(a[2]);return c(B,J(g),h);case
3:var
i=a[1],j=J(a[2]);return c(B,J(i),j);case
4:var
a=a[1];continue;default:return b(x[2][5],a[1])}}function
aG(a){if(a){var
b=a[1],d=aG(a[2]),e=c(B,J(b[3]),d);return c(B,J(b[2]),e)}return x[2][1]}function
N(b){var
a=b;for(;;){if(typeof
a!=="number")switch(a[0]){case
0:return aG([0,a[2],0]);case
1:var
a=a[1];continue;case
2:var
d=a[2],e=N(a[3]);return c(B,N(d),e);case
3:var
f=a[2],g=N(a[3]);return c(B,N(f),g);case
4:var
h=a[2],i=N(a[3]);return c(B,N(h),i)}return x[2][1]}}function
a5(b,a){var
d=a[1],e=c(v[8],a[2],b);function
g(a){var
d=a[1];return[0,d,c(v[8],a[2],b)]}return[0,c(f[17][15],g,d),e]}function
bV(b,a){function
d(p,o){var
b=p,a=o;for(;;){if(b){if(a){var
f=a[2],j=a[1],k=j[2],h=j[1],i=b[2],l=b[1],m=l[2],e=l[1];if(e===h){var
n=c(v[6],m,k);if(c(g[17],n,g[5])){var
b=i,a=f;continue}return[0,[0,e,n],d(i,f)]}return c(v[19],e,h)?[0,[0,e,m],d(i,a)]:[0,[0,h,k],d(b,f)]}return b}return a}}var
e=c(v[6],b[2],a[2]);return[0,d(b[1],a[1]),e]}function
$(h){var
a=h;for(;;)switch(a[0]){case
0:return[0,0,a[1]];case
1:var
i=a[1],j=$(a[2]);return bV($(i),j);case
2:var
b=a[1],e=a[2];if(0===e[0])var
d=e[1],f=b;else{if(0!==b[0])throw[0,ag,ga];var
d=b[1],f=a[2]}return c(g[17],d,g[5])?[0,0,v[11]]:a5(d,$(f));case
3:var
a=[1,a[1],[4,a[2]]];continue;case
4:var
k=$(a[1]);return a5(v[14],k);default:return[0,[0,[0,a[1],g[6]],0],g[5]]}}function
bW(i,b,a){var
d=a[2],e=a[1];function
g(a){return[0,a[2],a[1]]}var
h=c(f[17][15],g,e);return[0,a0(0),b,h,d]}function
bX(a){switch(a){case
0:return 5;case
1:return 3;case
2:return 4;case
3:return 1;case
4:return 2;default:return 0}}function
bY(j,g,h,c,a){var
i=g[1],k=g[4],l=g[3],m=g[2];function
d(e,d){var
g=bW(j,d,$(e));return[0,h,c,a,[0,l,b(f[17][9],k)],i,m,g]}try{var
n=i?bX(h):h;switch(n){case
0:var
e=d([1,c,[4,a]],0);break;case
1:var
e=d([1,a,[4,c]],1);break;case
2:var
e=d([1,c,[4,a]],1);break;case
3:var
e=d([1,[1,c,[0,v[14]]],[4,a]],1);break;case
4:var
e=d([1,[1,a,[0,v[14]]],[4,c]],1);break;default:var
e=d([1,c,[4,a]],2)}return e}catch(a){a=u(a);if(b(gb[4],a))throw a;throw a}}function
bZ(c,b,a){return[2,c,b,a]}function
a6(c,b,a){return[3,c,b,a]}function
b0(c,b,a){return[4,c,b,a]}function
a7(a,e,d,b){var
f=K(a,d);return c(e,f,K(a,b))}function
K(c,d){var
a=b(e[46][7],d);if(typeof
a==="number")return[5,a1(d,c)];else
switch(a[0]){case
0:var
j=a[2],k=a[1];return a7(c,function(b,a){return[1,b,a]},k,j);case
1:var
f=a[2],g=a[1],h=b(e[46][9],g);if(h){var
l=h[1];return[2,[0,l],K(c,f)]}var
i=b(e[46][9],f);if(i){var
m=[0,i[1]];return[2,K(c,g),m]}return[5,a1(d,c)];case
2:var
n=a[2],o=a[1];return a7(c,function(b,a){return[3,b,a]},o,n);case
3:return[4,K(c,a[1])];case
4:var
p=[0,v[12]];return[1,K(c,a[1]),p];default:return[0,a[1]]}}function
aj(c,a,g,o,j,n,m,k){var
h=a[4],i=a[3],d=a[2],p=a[1],e=bC(c),q=g?[0,[0,e],d]:d,r=g?[0,[1,e],d]:d;if(g){var
s=[0,i,b(f[17][9],h)];l(z[5],c[6],e,s)}var
t=aa(c,[0,o,q,i,[0,0,h]],j,m);return l(n,e,t,aa(c,[0,p,r,i,[0,1,h]],j,k))}function
S(a,g,f,e,d,c){var
h=K(a,d),b=bY(a,g,e,h,K(a,c));bM(a,b);return[0,f,b]}function
aa(d,b,h,g){var
f=b[1],k=b[4],l=b[3],m=b[2],a=c(e[46][8],h,g);if(typeof
a==="number")switch(a){case
0:return 0;case
1:return 1;default:return[5,g]}else
switch(a[0]){case
0:return S(d,b,g,0,a[1],a[2]);case
1:return S(d,b,g,5,a[1],a[2]);case
2:return S(d,b,g,4,a[1],a[2]);case
3:return S(d,b,g,1,a[1],a[2]);case
4:return S(d,b,g,3,a[1],a[2]);case
5:return S(d,b,g,2,a[1],a[2]);case
6:return[1,aa(d,[0,1-f,m,l,[0,2,k]],h,a[1])];case
7:return aj(d,b,1-f,f,h,bZ,a[1],a[2]);case
8:return aj(d,b,f,f,h,a6,a[1],a[2]);case
9:return aj(d,b,1-f,1-f,h,b0,a[1],a[2]);default:var
i=a[2],j=a[1],n=c(t[48],i,j);return aj(d,b,f,f,h,a6,c(t[48],j,i),n)}}function
b1(e,d,a){b(k[2],gc);l(k[2],gd,A,d);function
g(a){var
c=a[2],d=b(s[1][8],a[1]);return bh(k[2],ge,d,A,c)}c(f[17][14],g,a);return bE(e)}function
b2(d,a){var
h=b(aY[48][6],a),i=b(F[bm][1],h),j=b(aY[48][13],a);function
k(a){var
c=a[1];return[0,c,b(F[bm][1],a[2])]}var
l=c(f[17][15],k,j),e=aa(d,[0,1,0,af,gf],a,i);function
m(b){var
c=b[1];return[0,c,aa(d,[0,0,0,c,0],a,b[2])]}var
g=c(f[17][15],m,l);if(Y[1])b1(d,e,g);return[0,e,g]}function
bg(e,b,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,[0,a[2],b],0];case
1:var
d=a[1];return e<50?bf(e+1|0,b,d):cg(bf,[0,b,d]);case
2:var
g=a[3],h=O(b,a[2]),i=O(b,g);return c(f[18],h,i);case
3:var
j=a[3],k=O(b,a[2]),l=function(a){return O(a,j)};return c(f[17][bl],l,k);case
4:var
m=a[3],n=T(b,a[2]),o=O(b,m);return c(f[18],n,o)}return[0,b,0]}function
bf(e,b,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,[0,a[2],b],0];case
1:var
d=a[1];return e<50?bg(e+1|0,b,d):cg(bg,[0,b,d]);case
2:var
g=a[3],h=T(b,a[2]),i=function(a){return T(a,g)};return c(f[17][bl],i,h);case
3:var
j=a[3],k=T(b,a[2]),l=T(b,j);return c(f[18],k,l);case
4:var
m=a[3],n=O(b,a[2]),o=function(a){return T(a,m)};return c(f[17][bl],o,n)}return[0,b,0]}function
O(a,b){return ch(bg(0,a,b))}function
T(a,b){return ch(bf(0,a,b))}function
a8(a){if(a){var
b=a[2],c=O(0,a[1][2]),d=a8(b);return l(f[17][129],f[18],c,d)}return gg}function
aH(a){return 0===a[0]?c(k[2],gh,a[1]):c(k[2],gi,a[1])}function
b3(a){function
g(a){var
g=e[7],i=p(g),t=q===i?g[1]:d===i?b(o[2],g):g;A(j[27],[0,t,a]);b(j[35],0);var
h=a[7],m=b(v[31],h[2]),n=[0,h[3],h[4]];function
r(a){return b(v[29],aE)}hq(k[2],gj,h[1],r,n,m);b(k[2],gk);c(f[17][14],aH,a[6]);var
u=a[4][2];function
w(a){switch(a){case
0:return gl;case
1:return gm;default:return gn}}var
x=c(f[17][15],w,u),y=c(f[15][7],go,x);c(k[2],gp,y);var
z=a[5]?gq:gs,B=b(s[1][8],a[4][1]);return l(k[2],gr,B,z)}function
h(a){b(k[2],gt);return c(f[17][14],g,a)}return c(f[17][14],h,a)}function
ak(e){var
a=e;for(;;){if(a){var
d=a[2],b=a[1];switch(b[0]){case
6:var
f=b[1],g=ak(d);return c(x[2][4],f[1],g);case
15:var
h=b[2][2],i=ak(b[3][2]);return c(B,ak(h),i);default:var
a=d;continue}}return x[2][1]}}function
b4(b,a){return ci(b[5],a[5])}var
ab=b(f[20][1],[0,b4]);function
al(d){var
a=d;for(;;){if(a){var
b=a[1];switch(b[0]){case
5:var
e=b[1],f=al(a[2]);return c(ab[4],e,f);case
15:if(!a[2]){var
g=b[2][2],h=al(b[3][2]),i=al(g);return c(ab[7],i,h)}break}var
a=a[2];continue}return ab[1]}}function
aI(a){if(0===a[0])return al(a[1][3]);var
b=a[2],d=aI(a[3]),e=aI(b);return c(ab[7],e,d)}function
b5(g,c){function
h(a,c){var
k=c[4],l=c[3],m=c[2],r=c[1],h=bO(a[2]),i=bP(g,h);bK(a[5],i,g);var
f=e[46][1],j=p(f),s=q===j?f[1]:d===j?b(o[2],f):f,t=n(e[1],[0,s,i]),u=b(F[8],t);return[0,[0,a[5],r],[0,u,m],[0,[0,h,[5,a[5]]],l],[0,[1,a[2][1]],k]]}var
i=aI(c),a=l(ab[15],h,i,gu),j=a[3],k=a[2],m=a[1],r=b(f[17][9],a[4]),s=b(f[17][9],j),t=b(f[17][9],k);return[0,b(f[17][9],m),t,s,r]}function
a9(c,a){if(a){var
e=a[2],g=a[1];try{var
j=a3(c,g)[6],d=j}catch(a){a=u(a);if(a!==M)throw a;var
d=0}var
h=a9(c,e),i=b(f[17][9],d);return l(f[17][59],aC,i,h)}return 0}function
b6(a){function
d(c,a){var
d=c[2],e=b(f[17][1],a[2]);return ci(b(f[17][1],d),e)}try{var
e=c(f[17][46],d,a),g=b(f[17][5],e);return g}catch(a){a=u(a);if(a[1]===gv)return b(j[2],gw);throw a}}function
b7(d,a){function
e(g){var
a=g;for(;;){if(a){var
c=a[2],b=a[1];if(l(f[17][55],aC,b,d)){var
a=c;continue}var
h=bD(b);if(l(f[17][55],aC,h,d))throw j[3];return[0,b,e(c)]}return 0}}function
b(a){var
b=a[2],c=a[1];try{var
d=[0,[0,c,e(b)]];return d}catch(a){a=u(a);if(a===j[3])return 0;throw a}}return c(f[17][70],b,a)}function
aJ(a){if(0===a[0])return a[1][2];var
b=a[2],d=aJ(a[3]);return c(B,aJ(b),d)}function
b8(l,a){function
b(a){if(typeof
a==="number")return 0===a?[5,n(e[5],[0])]:[5,n(e[6],[0])];else
switch(a[0]){case
0:var
m=a[1];return c(x[2][3],a[2][7][1],l)?a:[5,m];case
1:var
d=b(a[1]);if(typeof
d!=="number"&&5===d[0])return[5,n(e[3],[0,d[1]])];return[1,d];case
2:var
o=a[3],p=a[1],f=b(a[2]),g=b(o);if(typeof
f!=="number"&&5===f[0])if(typeof
g!=="number"&&5===g[0])return[5,n(e[4],[0,f[1],g[1]])];return[2,p,f,g];case
3:var
q=a[3],r=a[1],h=b(a[2]),i=b(q);if(typeof
h!=="number"&&5===h[0])if(typeof
i!=="number"&&5===i[0])return[5,n(e[2],[0,h[1],i[1]])];return[3,r,h,i];case
4:var
s=a[3],u=a[1],j=b(a[2]),k=b(s);if(typeof
j!=="number"&&5===j[0])if(typeof
k!=="number"&&5===k[0])return[5,c(t[48],j[1],k[1])];return[4,u,j,k];default:return a}}return b(a)}function
aK(d,a){if(0===a[0]){var
e=a[1],g=b(x[2][21],e[2]),h=c(f[17][15],j[21],g),i=c(f[15][7],gx,h),m=l(k[4],gy,e[1],i);return c(j[54],d,m)}return cf(k[1],d,gz,a[1],aK,a[2],aK,a[3])}function
aL(a,d){function
e(g,d,c){if(c){var
h=c[1];if(0===h[0]){var
i=h[1],k=c[2],l=aL(a,b(f[17][9],[0,[1,i],d]));return[1,i,e(g,[0,[0,i],d],k),l]}var
j=h[1],m=e(g,[0,[1,j],d],c[2]);return[1,j,aL(a,b(f[17][9],[0,[0,j],d])),m]}return[0,g]}var
g=b7(d,a);try{var
h=b6(g)}catch(e){e=u(e);var
i=b(f[17][1],a),j=b(f[17][1],g);l(k[2],gA,j,i);c(f[17][14],aH,d);throw e}var
m=h[2],n=h[1];return e(n,b(f[17][9],d),m)}function
a_(i,m){var
d=0,a=m,n=i[2],o=i[1];a:for(;;){if(a){var
k=a[1];if(0===k[0]){var
l=k[1],r=a[2],t=l[2];if(c(s[1][1],o,l[1])){var
e=[0,t,n];for(;;){var
f=e[1];if(f){var
g=e[2];if(g){var
p=g[2],q=f[2];if(bz(f[1],g[1])){var
e=[0,q,p];continue}}var
h=0}else
var
h=[0,e[2]];if(h)return[0,d,h[1]];var
d=d+1|0,a=r;continue a}}}var
d=d+1|0,a=a[2];continue}return b(j[2],gB)}}function
a$(h){function
i(h){switch(h){case
0:var
a=e[35],f=p(a),i=q===f?a[1]:d===f?b(o[2],a):a;return[0,i];case
1:var
c=e[36],g=p(c),j=q===g?c[1]:d===g?b(o[2],c):c;return[0,j];default:return 0}}var
j=c(f[17][70],i,h),a=e[34],g=p(a),k=q===g?a[1]:d===g?b(o[2],a):a;return c(e[44],k,j)}function
y(h,f){var
d=0,a=h;for(;;){if(a){var
g=a[1];if(1===g[0])if(f===g[1])return b(e[42],d);var
d=d+1|0,a=a[2];continue}var
i=c(k[4],gC,f);return b(j[2],i)}}function
ba(k,j,i){var
l=[0,b(e[42],0)],a=e[28],f=p(a),m=q===f?a[1]:d===f?b(o[2],a):a,n=b(C,[0,m,l]),r=k?v[14]:g[6],s=b(e[46][6],r),t=[0,b(e[46][6],g[6]),j,s,i,n],c=e[31],h=p(c),u=q===h?c[1]:d===h?b(o[2],c):c;return b(C,[0,u,t])}function
P(h,c,L){var
f=L;for(;;){if(f){var
a=f[1];switch(a[0]){case
0:var
x=f[2],w=a[3],v=a[1],i=1;break;case
1:if(!f[2]){var
R=a[1],S=b(e[46][6],a[2]),T=[0,y(c,R[1]),S],l=e[30],A=p(l),U=q===A?l[1]:d===A?b(o[2],l):l;return b(C,[0,U,T])}var
i=0;break;case
3:var
x=f[2],w=a[2],v=a[1],i=1;break;case
4:var
B=a[3],D=a[2],V=B[2],W=B[1],X=D[2],Y=D[1],Z=P(h,[0,[1,a[1]],c],f[2]),_=y(c,V[1]),$=b(e[46][6],W),aa=y(c,X[1]),ab=[0,b(e[46][6],Y),aa,$,_,Z],m=e[31],E=p(m),ac=q===E?m[1]:d===E?b(o[2],m):m;return b(C,[0,ac,ab]);case
5:var
n=a[1],ad=n[4],ae=n[3],af=n[2],ag=P(h,[0,[1,n[1][1]],c],f[2]),ah=y(c,af[1]),ai=b(e[46][6],ad),aj=y(c,ae[1]),ak=[0,b(e[46][6],g[6]),aj,ai,ah,ag],r=e[31],F=p(r),al=q===F?r[1]:d===F?b(o[2],r):r;return b(C,[0,al,ak]);case
6:var
f=f[2];continue;case
9:if(!f[2]){var
am=a[1],an=y(c,a[2][1]);return ba(0,y(c,am[1]),an)}var
i=0;break;case
10:if(!f[2]){var
ao=a[3],ap=a[1],aq=y(c,a[2][1]);return ba(ao,y(c,ap[1]),aq)}var
i=0;break;case
11:var
ar=a[3],as=a[2],at=P(h,[0,[1,a[1]],c],f[2]),au=y(c,ar),av=[0,y(c,as[1]),au,at],s=e[32],G=p(s),aw=q===G?s[1]:d===G?b(o[2],s):s;return b(C,[0,aw,av]);case
15:var
I=a[3],J=a[2],az=I[2],aA=I[1],aB=a[1],aC=P(h,[0,[1,J[1]],c],J[2]),aD=P(h,[0,[1,aA],c],az),aE=[0,y(c,aB[1]),aC,aD],u=e[33],K=p(u),aF=q===K?u[1]:d===K?b(o[2],u):u;return b(C,[0,aF,aE]);case
16:return b(j[2],gE);case
12:case
13:case
14:if(!f[2]){var
ax=[0,y(c,a[1])],t=e[28],H=p(t),ay=q===H?t[1]:d===H?b(o[2],t):t;return b(C,[0,ay,ax])}var
i=0;break;default:var
f=f[2];continue}if(i){var
M=P(h,c,x),N=b(e[46][6],w),O=[0,y(c,v[1]),N,M],k=e[29],z=p(k),Q=q===z?k[1]:d===z?b(o[2],k):k;return b(C,[0,Q,O])}}return b(j[2],gD)}}function
bb(h,f,d,a){if(a){var
i=a[1],m=a[2];try{var
v=c(z[7],f[5],i),g=v}catch(a){a=u(a);if(a!==M)throw a;var
o=c(k[4],gI,i),g=b(j[2],o)}var
l=a_(g[4],d),p=l[2],q=l[1],r=bb(h,f,[0,[1,g[7][1]],d],m),s=a$(p),t=[0,b(e[42],q),s,r];return n(e[38],t)}var
w=[0,P(f,d,h)];return n(e[39],w)}function
aM(h,g,d){if(0===d[0]){var
i=d[1],o=b(x[2][21],i[2]);return bb(i[3],h,g,o)}var
l=d[1],p=d[3],q=d[2];try{var
E=c(z[7],h[6],l),a=E}catch(d){d=u(d);if(d!==M)throw d;var
r=c(k[4],gF,l),a=b(j[2],r)}var
m=a_(a,g),s=m[2],t=m[1],v=c(f[18],a[2],gG),w=[0,[0,a[1],v]],y=c(f[18],a[2],gH),A=aM(h,[0,[0,[0,a[1],y]],g],p),B=aM(h,[0,w,g],q),C=a$(s),D=[0,b(e[42],t),C,B,A];return n(e[37],D)}function
b9(i,e,h){function
l(a){return a[7]}var
m=c(f[17][15],l,h),a=c(v[68],[0,a0,bI,aE],m),d=ak(a),g=a9(i,b(x[2][21],d));if(Y[1]){c(k[2],gJ,e);c(v[36],aE,a);b(j[30],gK);var
n=function(a){return c(k[2],gL,a)};c(x[2][13],n,d);b(j[30],gM);c(f[17][14],aH,g);b(k[2],gN)}return[0,[0,e,d,a],g]}function
b_(K,a,v,J){var
w=v[1],L=v[2];if(Y[1])b(k[2],gO);function
M(b,c){return b9(a,b,c)}var
g=aL(c(f[17][16],M,J),0);if(Y[1]){aK(j[27],g);b(j[35],0)}var
y=aJ(g),O=b(x[2][21],y);function
P(b){return a3(a,b)}var
A=c(f[17][15],P,O),Q=s[1][10][1];function
R(b,a){return c(s[1][10][4],a[4][1],b)}var
S=b(c(f[17][18],R,Q),A),T=c(s[1][10][6],af,S),u=b(s[1][10][21],T);function
V(a){return l(f[17][cv],s[1][1],a,L)}var
W=c(f[17][15],V,u),X=N(w),Z=c(B,aG(A),X),h=b5(a,g),$=h[4],aa=h[3],ab=h[2],ac=h[1],ad=b(x[2][21],Z),ae=c(f[18],ad,ac);function
C(d,c){if(c){var
e=c[1],f=c[2],g=b(a2(a),e);l(z[5],a[3],e,d);return[0,g,C(d+1|0,f)]}return 0}var
ag=C(0,ae),i=e[46][1],D=p(i),ah=q===D?i[1]:d===D?b(o[2],i):i,ai=c(e[44],ah,ag);function
aj(b){return bU(a,b)}var
ak=c(f[17][15],aj,aa),al=a4(a,w);function
am(b){return a4(a,b8(y,b))}var
an=c(f[17][15],am,W),ao=b(e[45],a[2]),ap=c(f[18],ak,an),m=e[14],E=p(m),aq=q===E?m[1]:d===E?b(o[2],m):m,ar=[0,al,ao,ai,c(e[44],aq,ap)],as=n(e[40],ar);function
at(a){return[0,[0,a,0]]}var
au=c(f[17][15],at,u),av=aM(a,c(f[18],[0,[0,[0,af,0]],$],au),g),r=e[7],G=p(r),aw=q===G?r[1]:d===G?b(o[2],r):r,ax=b(F[8],aw),ay=b(U[85],ax);if(K)var
t=e[5],H=p(t),az=0,aA=q===H?t[1]:d===H?b(o[2],t):t,aB=b(F[8],aA),I=c(U[5],aB,az);else
var
I=U[66];var
aC=n(e[41],[0,av]),aD=b(F[8],aC),aE=b(U[85],aD),aF=b(F[8],as),aH=c(U[5],aF,2),aI=c(f[17][15],F[10],u),aN=c(f[18],ab,aI);return c(_,c(_,c(_,c(_,c(_,b(U[147],aN),aH),aE),by),I),ay)}var
bc=[0,v,ft,fu,z,Y,by,fy,_,C,bz,bA,aC,af,bB,bC,bD,fA,bE,a0,bF,bI,bH,bG,aE,a1,a2,bK,bL,fL,bM,a3,D,bN,A,bO,n,bP,bQ,bR,ah,bS,bT,ai,a4,bU,B,J,aG,N,a5,bV,$,bW,bX,bY,bZ,a6,b0,K,a7,aj,S,aa,b1,b2,O,T,a8,aH,b3,ak,b4,ab,al,aI,b5,a9,b6,b7,aJ,b8,aK,aL,a_,a$,y,ba,P,aM,bb,b9,b_,function(e){function
a(g){b(E[3],gP);bF(0);bH(0);try{var
a=bB(0),c=b2(a,g),i=c[2],j=c[1];bG(b(f[17][1],a[1])-1|0);var
d=a8([0,[0,af,[1,j]],i]);if(Y[1])b3(d);var
k=b_(e,a,c,d);return k}catch(a){a=u(a);if(a===v[28]){var
h=b(r[3],gQ);return l(b$[6],0,0,h)}throw a}}return b(ca[63][8],a)}];aN(249,bc,"Romega_plugin.Refl_omega");b(bd[12],Q);function
am(a){var
d=c(be[17],s[1][6],gR),e=b(s[5][4],d),f=b(s[6][4],a),g=c(s[13][2],[0,e],f),h=b(V[6][6],g);return b(V[12][21],h)}function
an(d,a){var
e=c(f[17][103],gS[33],a);function
g(a){if(h(a,gT)){if(h(a,gU)){if(h(a,gV)){if(h(a,gW)){var
d=c(j[16],gX,a),e=b(r[3],d);return l(b$[6],0,0,e)}return am(gY)}return am(gZ)}return am(g0)}return am(g1)}var
i=c(be[17],g,e),k=b(bc[93],d),m=c(Z[66][3],U[28],k),n=b(Z[66][20],i),o=b(ca[56],n),p=b(Z[66][29],o);return c(Z[66][3],p,m)}var
g2=0,g4=[0,function(a){return a?b(j[2],g3):function(a){return an(1,0)}},g2],g6=[0,function(a){return a?b(j[2],g5):function(a){return an(0,0)}},g4],g7=b(cb[12],g6);l(V[6][9],0,[0,Q,g8],g7);function
g9(a){return l(V[9][4],[0,Q,g$],0,g_)}c(bd[19],g9,Q);var
ha=0,hd=[0,function(a){return a?b(j[2],hb):function(a){return an(0,hc)}},ha],hf=[0,function(a){if(a)if(!a[2]){var
d=a[1],e=b(cd[17],cc[9]),f=b(cd[6],e),g=c(V[12][2][7],f,d);return function(a){return an(0,c(be[17],s[1][8],g))}}return b(j[2],he)},hd],hg=b(cb[12],hf);l(V[6][9],0,[0,Q,hh],hg);function
hi(g){var
e=[0,b(s[1][7],hk)],a=cc[9],d=0;if(0===a[0]){var
f=[0,[0,ho,[0,hn,[0,[1,c(hm[10],0,[0,[0,[5,[0,a[1]]]],e])],d]]],hj];return l(V[9][4],[0,Q,hp],0,f)}throw[0,ag,hl]}c(bd[19],hi,Q);var
ce=[0,Q,am,an];aN(258,ce,"Romega_plugin.G_romega");aN(259,[0,e,bc,ce],"Romega_plugin");return});
