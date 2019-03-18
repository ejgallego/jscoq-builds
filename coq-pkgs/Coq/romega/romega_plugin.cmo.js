function(hd){"use strict";var
cg="xO",cl=" - ",cm="not",r=250,ck="Z.succ",cw="Z.pred",bl="Z.opp",co="xH",aU="Zpos",cv="REIFED PROBLEM\n\n",d=246,cj="FF",bi="Z.sub",cr="True",aS="Z",Z="romega",ak="plugins/romega/refl_omega.ml",cu="\n====================================\n",bk="N",cz="False",cq="with",bj="Z.mul",cy="Prop",aT="",cn="or",ct="TT",aV="Omega",cf="=SYSTEM===================================\n",aR="Zneg",bh="Z.add",ci="positive",aQ="Z0",cs="  Depends on:",cx="nat",cp="and",ch="\n------------------------------------\n",ce="xI",R="Coq",w=hd.jsoo_runtime,cd=w.caml_int_compare,a=w.caml_new_string,q=w.caml_obj_tag,aP=w.caml_register_global,h=w.caml_string_notequal,cc=w.caml_trampoline,cb=w.caml_trampoline_return,u=w.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):w.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):w.caml_call_gen(a,[b,c])}function
o(a,b,c,d){return a.length==3?a(b,c,d):w.caml_call_gen(a,[b,c,d])}function
aO(a,b,c,d,e){return a.length==4?a(b,c,d,e):w.caml_call_gen(a,[b,c,d,e])}function
bg(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):w.caml_call_gen(a,[b,c,d,e,f])}function
Y(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):w.caml_call_gen(a,[b,c,d,e,f,g])}function
ca(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):w.caml_call_gen(a,[b,c,d,e,f,g,h])}var
j=w.caml_get_global_data(),aM=a("romega_plugin"),p=j.CamlinternalLazy,k=j.EConstr,g=j.Bigint,O=j.Option,aa=j.Proofview,az=j.Tacmach,t=j.Names,bq=j.Univ,D=j.Coqlib,_=j.UnivGen,aX=j.Nametab,f=j.Util,l=j.Stdlib,i=j.Stdlib__printf,x=j.Int,ac=j.Assert_failure,H=j.Not_found,s=j.Pp,b_=j.CErrors,Q=j.Tactics,bH=j.Invalid_argument,T=j.Tacticals,bc=j.Stdlib__list,aN=j.Ltac_plugin,bm=[0,a(R),[0,a(Z),[0,a("ReflOmegaCore"),0]]],e_=a(bh),e$=a(bj),fa=a(bl),fb=a(cw),fc=a(bi),fd=a(ck),fe=a(aQ),ff=a(aR),fg=a(aU),e4=a("Z.ge"),e5=a("Z.gt"),e6=a("Z.le"),e7=a("Z.lt"),e8=a("Zne"),e9=a("eq"),eT=a(bh),eU=a(bj),eV=a(bl),eW=a(cw),eX=a(bi),eY=a(ck),eZ=a(aQ),e0=a(aR),e1=a(aU),eP=a(aQ),eQ=a(aR),eR=a(aU),eM=a(co),eN=a(ce),eO=a(cg),eK=a(bi),eI=a(bl),eG=a(bj),eE=a(bh),eC=a("Npos"),eB=a("N0"),eA=a(aR),ez=a(aU),ey=a(aQ),ex=a(ce),ew=a(cg),ev=a(co),eu=a(aS),eo=a(cz),ep=a(cr),eq=a(cp),er=a("iff"),es=a(cm),et=a(cn),ej=a("nil"),ei=a("cons"),ee=[0,a("Init"),[0,a("Datatypes"),0]],ef=a(aT),ed=a("O"),ec=a("S"),ea=a("do_omega"),d_=a("interp_goal_concl"),d8=a("E_SOLVE"),d6=a("E_EXTRACT"),d4=a("E_SPLIT"),d2=a("D_right"),d0=a("D_left"),dY=a("direction"),dW=a("O_SPLIT_INEQ"),dU=a("O_MERGE_EQ"),dS=a("O_SUM"),dQ=a("O_NOT_EXACT_DIVIDE"),dO=a("O_DIVIDE"),dM=a("O_BAD_CONSTANT"),dK=a("Tprop"),dI=a("Timp"),dG=a("Tand"),dE=a("Tor"),dC=a("Tnot"),dA=a("FalseTerm"),dy=a("TrueTerm"),dw=a("NeqTerm"),du=a("GtTerm"),ds=a("LtTerm"),dq=a("GeqTerm"),dn=a("LeqTerm"),dl=a("EqTerm"),dj=a("proposition"),dh=a("Tvar"),df=a("Tminus"),dd=a("Topp"),db=a("Tmult"),c$=a("Tplus"),c9=a("Tint"),c7=a("I"),c5=a(cz),c3=a(cr),c1=a(cn),cZ=a(cm),cX=a(cp),cV=a("eq_refl"),cU=a(aV),cT=a(aV),cS=a(aV),cR=a(aV),cB=a("."),cC=a(aT),cD=a(aT),cA=[0,a(aS),[0,a(bk),[0,a("Pos"),0]]],cE=a("Const_omega.DestConstApp"),cF=[0,a(R),[0,a("Logic"),[0,a("Decidable"),0]]],cG=[0,a("ZOmega"),0],cJ=[0,[0,a(R),[0,a("Lists"),[0,a("List"),0]]],0],cP=[0,[0,a(R),[0,a("Numbers"),[0,a("BinNums"),0]]],0],cQ=[0,[0,a(R),[0,a("ZArith"),[0,a("BinInt"),0]]],0],fE=[0,[2,0,0],a("%s")],fF=[0,[12,40,[15,[11,a(" + "),[15,[12,41,0]]]]],a("(%a + %a)")],fG=[0,[12,40,[15,[11,a(" * "),[15,[12,41,0]]]]],a("(%a * %a)")],fH=[0,[12,40,[15,[11,a(cl),[15,[12,41,0]]]]],a("(%a - %a)")],fI=[0,[11,a("~ "),[15,0]],a("~ %a")],fJ=[0,[12,86,[4,0,[0,2,2],0,0]],a("V%02d")],fQ=[0,[11,a(ct),0],a(ct)],fR=[0,[11,a(cj),0],a(cj)],fS=[0,[15,[12,32,[2,0,[12,32,[15,0]]]]],a("%a %s %a")],fT=[0,[11,a("not("),[15,[12,41,0]]],a("not(%a)")],fU=[0,[12,40,[15,[11,a(" or "),[15,[12,41,0]]]]],a("(%a or %a)")],fV=[0,[12,40,[15,[11,a(" and "),[15,[12,41,0]]]]],a("(%a and %a)")],fW=[0,[12,40,[15,[11,a(" => "),[15,[12,41,0]]]]],a("(%a => %a)")],fX=[0,[11,a(cy),0],a(cy)],f2=[0,a(ak),440,15],f8=[0,0,0],gn=a(" "),go=[0,[4,0,0,0,[12,91,[2,0,[12,93,0]]]],a("%d[%s]")],gp=[0,[12,83,[4,0,0,0,[12,40,[15,[12,44,[15,[12,41,0]]]]]]],a("S%d(%a,%a)")],gq=[0,[4,0,0,0,[11,a(cl),[4,0,0,0,[12,10,0]]]],a("%d - %d\n")],gu=a("not_treated"),gt=a("bad history"),gv=[0,[11,a("Cannot find constructor "),[4,0,0,0,0]],a("Cannot find constructor %d")],gw=[0,0,0],gx=[0,1,0],gy=[0,[11,a("Cannot find equation "),[4,0,0,0,0]],a("Cannot find equation %d")],gG=[0,a(R),[0,a(Z),[0,a("ROmega"),0]]],gH=a("ROmega can't solve this system"),gF=[0,a(ak),1020,33],gE=[0,[11,a(cu),0],a(cu)],gB=[0,[12,32,[4,0,0,0,0]],a(" %d")],gz=[0,[11,a("SYSTEME "),[4,0,0,0,[12,10,0]]],a("SYSTEME %d\n")],gA=a("\n  Depend :"),gC=a("\n  Split points :"),gD=[0,[11,a(ch),0],a(ch)],gs=[0,[11,a("get_hyp "),[4,0,0,0,0]],a("get_hyp %d")],gr=a("find_path"),gm=a("select_smaller"),gk=[0,0,0,0,0],gj=[0,[11,a(cf),0],a(cf)],gb=a("L"),gc=a("R"),gd=a("M"),ga=[0,[11,a(cs),0],a(cs)],ge=a(aT),gf=[0,[11,a("\n  Path: "),[2,0,0]],a("\n  Path: %s")],gg=a("yes"),gi=a("no"),gh=[0,[11,a("\n  Origin: "),[2,0,[11,a(" (negated : "),[2,0,[11,a(")\n\n"),0]]]]],a("\n  Origin: %s (negated : %s)\n\n")],f$=[0,[11,a("  E"),[4,0,0,0,[11,a(" : "),[15,[12,32,[2,0,[11,a(" 0\n"),0]]]]]]],a("  E%d : %a %s 0\n")],f9=[0,[11,a(" L"),[4,0,0,0,0]],a(" L%d")],f_=[0,[11,a(" R"),[4,0,0,0,0]],a(" R%d")],f7=[0,2,0],f6=[0,[11,a("  "),[2,0,[11,a(": "),[15,[12,10,0]]]]],a("  %s: %a\n")],f4=[0,[11,a(cv),0],a(cv)],f5=[0,[11,a("  CONCL: "),[15,[12,10,0]]],a("  CONCL: %a\n")],f1=[0,a(ak),348,9],f0=[0,a(ak),323,9],fZ=[0,[4,0,0,0,[11,a(" -> "),[4,0,0,0,[12,10,0]]]],a("%d -> %d\n")],fY=[0,[11,a("Atome "),[4,0,0,0,[11,a(" non trouv\xc3\xa9\n"),0]]],a("Atome %d non trouv\xc3\xa9\n")],fK=a("="),fL=a("<="),fM=a(">="),fN=a(">"),fO=a("<"),fP=a("!="),fD=[0,[11,a("Omega Equation "),[4,0,0,0,[11,a(" non trouv\xc3\xa9e\n"),0]]],a("Omega Equation %d non trouv\xc3\xa9e\n")],fC=a("get_prop"),fA=[0,a(ak),234,2],fz=a("get_reified_atom"),fy=[0,[11,a("OV"),[4,0,0,0,0]],a("OV%d")],fr=[0,[12,40,[0,[4,0,[0,2,2],0,[12,41,0]]]],a("(%c%02d)")],ft=a(" := "),fu=a("  ===============================\n\n"),fv=a("ENVIRONMENT OF PROPOSITIONS :"),fw=a("ENVIRONMENT OF TERMS :"),fo=a("__goal__"),g4=[0,a(cx),[0,a(ci),[0,a(bk),[0,a(aS),0]]]],gL=a(bk),gM=a(aS),gN=a(cx),gO=a(ci),gQ=a("zify_positive"),gR=a("zify_nat"),gS=a("zify_op"),gT=a("zify_N"),gP=a("No ROmega knowledge base for type "),gJ=[0,a("PreOmega"),[0,a("omega"),[0,a(R),0]]],gU=[0,[0,a("Use lia instead.")]],gV=[0,[0,a("8.9")]],gY=[0,a("unsafe_romega"),0],g0=[0,a(Z),0],g2=a(Z),g5=[0,a(Z),[0,a(cq),[0,a("*"),0]]],g8=a("l"),g$=a(cq),ha=a(Z),hc=a("romega'"),e2=j.Reductionops,eh=j.Globnames,eg=j.Global,gl=j.Failure,f3=j.Logic,fq=j.Pfedit,fs=j.Printer,fx=j.Feedback,fh=j.Omega_plugin,fm=j.Stdlib__hashtbl,gK=j.Stdlib__string,gI=j.Mltop,gW=j.Vernacinterp,g9=j.Stdarg,g_=j.Genarg;function
aW(a){var
h=b(aX[40],a),d=b(t[5][5],h);if(d)var
e=b(t[1][8],d[1]),i=c(f[15][53][2],e,cA)?c(l[17],e,cB):cC,g=i;else
var
g=cD;var
j=b(aX[41],a),k=b(t[1][8],j);return c(l[17],g,k)}function
al(e,g){var
f=c(k[83],e,g),d=f[2],a=c(k[3],e,f[1]);switch(a[0]){case
1:if(!d)return[0,b(t[1][8],a[1])];break;case
6:if(!a[1])if(!d)return[2,a[2],a[3]];break;case
10:return[1,aW([1,a[1][1]]),d];case
11:return[1,aW([2,a[1][1]]),d];case
12:return[1,aW([3,a[1][1]]),d]}return 0}var
am=[248,cE,w.caml_fresh_oo_id(0)];function
bn(e,g){var
f=c(k[83],e,g),h=f[2],a=c(k[3],e,f[1]);switch(a[0]){case
10:var
d=[1,a[1][1]];break;case
11:var
d=[2,a[1][1]];break;case
12:var
d=[3,a[1][1]];break;default:throw am}return[0,b(aX[41],d),h]}var
cH=[0,c(l[26],bm,cG),0],cI=c(l[26],[0,bm,0],cH),cK=c(l[26],cJ,cI),cL=c(l[26],D[6],cK),cM=c(l[26],D[5],cL),cN=c(l[26],[0,cF,0],cM),cO=c(l[26],D[7],cN);function
F(a){var
c=o(D[4],cR,D[7],a),d=b(_[21],c);return b(k[8],d)}function
m(a){var
c=o(D[4],cS,cO,a),d=b(_[21],c);return b(k[8],d)}function
an(a){var
c=o(D[4],cT,cQ,a),d=b(_[21],c);return b(k[8],d)}function
G(a){var
c=o(D[4],cU,cP,a),d=b(_[21],c);return b(k[8],d)}var
cW=[d,function(a){return F(cV)}],cY=[d,function(a){return F(cX)}],c0=[d,function(a){return F(cZ)}],c2=[d,function(a){return F(c1)}],c4=[d,function(a){return F(c3)}],c6=[d,function(a){return F(c5)}],c8=[d,function(a){return F(c7)}],c_=[d,function(a){return m(c9)}],da=[d,function(a){return m(c$)}],dc=[d,function(a){return m(db)}],de=[d,function(a){return m(dd)}],dg=[d,function(a){return m(df)}],di=[d,function(a){return m(dh)}],dk=[d,function(a){return m(dj)}],dm=[d,function(a){return m(dl)}],dp=[d,function(a){return m(dn)}],dr=[d,function(a){return m(dq)}],dt=[d,function(a){return m(ds)}],dv=[d,function(a){return m(du)}],dx=[d,function(a){return m(dw)}],dz=[d,function(a){return m(dy)}],dB=[d,function(a){return m(dA)}],dD=[d,function(a){return m(dC)}],dF=[d,function(a){return m(dE)}],dH=[d,function(a){return m(dG)}],dJ=[d,function(a){return m(dI)}],dL=[d,function(a){return m(dK)}],dN=[d,function(a){return m(dM)}],dP=[d,function(a){return m(dO)}],dR=[d,function(a){return m(dQ)}],dT=[d,function(a){return m(dS)}],dV=[d,function(a){return m(dU)}],dX=[d,function(a){return m(dW)}],dZ=[d,function(a){return m(dY)}],d1=[d,function(a){return m(d0)}],d3=[d,function(a){return m(d2)}],d5=[d,function(a){return m(d4)}],d7=[d,function(a){return m(d6)}],d9=[d,function(a){return m(d8)}],d$=[d,function(a){return m(d_)}],eb=[d,function(a){return m(ea)}],ao=[d,function(a){return F(ec)}],ap=[d,function(a){return F(ed)}];function
bo(a){if(0===a){var
c=q(ap);return r===c?ap[1]:d===c?b(p[2],ap):ap}var
e=q(ao),f=[0,bo(a-1|0)],g=r===e?ao[1]:d===e?b(p[2],ao):ao;return b(k[21],[0,g,f])}function
bp(c){var
a=o(D[2],ef,ee,c),d=b(eg[45],a)?function(a){var
c=b(bq[29][3],[0,a]);return b(k[2][1],c)}:function(a){return k[2][3]};return function(c){var
e=d(c),f=[0,b(eh[10],a),e];return b(k[28],f)}}function
br(d,c,a){function
e(a){if(a){var
h=a[1],i=[0,h,e(a[2])],f=[0,b(bp(ei),d),[0,c]],j=[0,b(k[21],f),i];return b(k[21],j)}var
g=[0,b(bp(ej),d),[0,c]];return b(k[21],g)}return e(a)}var
ek=b(_[3],0);function
el(a){return br(ek,k[14],a)}var
em=bq[1][1];function
en(a,b){return br(em,a,b)}var
$=[d,function(a){return G(eu)}],aq=[d,function(a){return G(ev)}],ar=[d,function(a){return G(ew)}],as=[d,function(a){return G(ex)}],at=[d,function(a){return G(ey)}],au=[d,function(a){return G(ez)}],av=[d,function(a){return G(eA)}],aw=[d,function(a){return G(eB)}],ax=[d,function(a){return G(eC)}];function
ay(a){if(c(g[17],a,g[6])){var
e=q(aq);return r===e?aq[1]:d===e?b(p[2],aq):aq}var
f=c(g[15],a,g[7]),l=f[2],m=[0,ay(f[1])];if(c(g[17],l,g[5]))var
h=q(ar),n=r===h?ar[1]:d===h?b(p[2],ar):ar,i=n;else
var
j=q(as),o=r===j?as[1]:d===j?b(p[2],as):as,i=o;return b(k[21],[0,i,m])}function
eD(a){if(0===a){var
c=q(aw);return r===c?aw[1]:d===c?b(p[2],aw):aw}var
e=q(ax),f=[0,ay(b(g[3],a))],h=r===e?ax[1]:d===e?b(p[2],ax):ax;return b(k[21],[0,h,f])}var
eF=[d,function(a){return an(eE)}],eH=[d,function(a){return an(eG)}],eJ=[d,function(a){return an(eI)}],eL=[d,function(a){return an(eK)}];function
bs(i,a){function
d(j){var
f=bn(i,j),a=f[2],e=b(t[1][8],f[1]);if(h(e,eM)){if(h(e,eN)){if(!h(e,eO))if(a)if(!a[2]){var
k=d(a[1]);return c(g[14],g[7],k)}}else
if(a)if(!a[2]){var
l=d(a[1]),m=c(g[14],g[7],l);return c(g[12],g[6],m)}}else
if(!a)return g[6];throw am}try{var
e=[0,d(a)];return e}catch(a){a=u(a);if(a===am)return 0;throw a}}function
bt(f,k){try{var
j=bn(f,k),d=j[2],i=b(t[1][8],j[1]);if(h(i,eP))if(h(i,eQ))if(h(i,eR))var
a=0;else
if(d)if(d[2])var
a=0;else
var
e=bs(f,d[1]),a=1;else
var
a=0;else
if(d)if(d[2])var
a=0;else
var
l=bs(f,d[1]),e=c(O[16],g[22],l),a=1;else
var
a=0;else
if(d)var
a=0;else
var
e=[0,g[5]],a=1;if(!a)var
e=0;return e}catch(a){a=u(a);if(a===am)return 0;throw a}}function
bu(a){if(c(g[17],a,g[5])){var
e=q(at);return r===e?at[1]:d===e?b(p[2],at):at}if(b(g[18],a)){var
f=q(au),i=[0,ay(a)],j=r===f?au[1]:d===f?b(p[2],au):au;return b(k[21],[0,j,i])}var
h=q(av),l=[0,ay(b(g[22],a))],m=r===h?av[1]:d===h?b(p[2],av):av;return b(k[21],[0,m,l])}function
eS(p,o){var
a=al(p,o);if(typeof
a!=="number"&&1===a[0]){var
c=a[1];if(h(c,eT))if(h(c,eU))if(h(c,eV))if(h(c,eW))if(h(c,eX))if(h(c,eY)){var
s=h(c,eZ)?h(c,e0)?h(c,e1)?1:0:0:0;if(!s){var
q=bt(p,o);return q?[5,q[1]]:0}}else{var
d=a[2];if(d)if(!d[2])return[4,d[1]]}else{var
e=a[2];if(e){var
f=e[2];if(f)if(!f[2])return[2,e[1],f[1]]}}else{var
i=a[2];if(i)if(!i[2]){var
r=i[1];return[0,r,bu(b(g[22],g[6]))]}}else{var
j=a[2];if(j)if(!j[2])return[3,j[1]]}else{var
k=a[2];if(k){var
l=k[2];if(l)if(!l[2])return[1,k[1],l[1]]}}else{var
m=a[2];if(m){var
n=m[2];if(n)if(!n[2])return[0,m[1],n[1]]}}}return 0}function
bv(f,e){var
a=q($),g=r===a?$[1]:d===a?b(p[2],$):$,h=e2[80];function
i(a){return c(h,0,a)}return aO(az[42][1],i,f,e,g)}function
e3(B,A){var
C=b(aa[67][5],B),c=al(C,A);if(typeof
c!=="number"&&1===c[0]){var
e=c[1];if(h(e,e4))if(h(e,e5))if(h(e,e6))if(h(e,e7))if(h(e,e8)){if(!h(e,e9)){var
n=c[2];if(n){var
o=n[2];if(o){var
p=o[2];if(p)if(!p[2]){var
D=p[1],E=o[1];if(bv(B,n[1]))return[0,E,D]}}}}}else{var
q=c[2];if(q){var
r=q[2];if(r)if(!r[2])return[1,q[1],r[1]]}}else{var
s=c[2];if(s){var
t=s[2];if(t)if(!t[2])return[2,s[1],t[1]]}}else{var
u=c[2];if(u){var
v=u[2];if(v)if(!v[2])return[3,u[1],v[1]]}}else{var
w=c[2];if(w){var
x=w[2];if(x)if(!x[2])return[4,w[1],x[1]]}}else{var
y=c[2];if(y){var
z=y[2];if(z)if(!z[2])return[5,y[1],z[1]]}}}var
a=al(C,A);if(typeof
a!=="number")switch(a[0]){case
1:var
d=a[1];if(h(d,eo)){if(h(d,ep))if(h(d,eq))if(h(d,er))if(h(d,es)){if(!h(d,et)){var
f=a[2];if(f){var
g=f[2];if(g)if(!g[2])return[7,f[1],g[1]]}}}else{var
i=a[2];if(i)if(!i[2])return[6,i[1]]}else{var
j=a[2];if(j){var
k=j[2];if(k)if(!k[2])return[10,j[1],k[1]]}}else{var
l=a[2];if(l){var
m=l[2];if(m)if(!m[2])return[8,l[1],m[1]]}}else
if(!a[2])return 0}else
if(!a[2])return 1;break;case
2:return[9,a[1],a[2]]}return 2}function
E(a,q){var
b=al(a,q);if(typeof
b!=="number"&&1===b[0]){var
d=b[1];if(h(d,e_))if(h(d,e$))if(h(d,fa))if(h(d,fb))if(h(d,fc))if(h(d,fd)){var
D=h(d,fe)?h(d,ff)?h(d,fg)?1:0:0:0;if(!D)return bt(a,q)}else{var
e=b[2];if(e)if(!e[2]){var
r=E(a,e[1]);return c(O[16],g[9],r)}}else{var
f=b[2];if(f){var
i=f[2];if(i)if(!i[2]){var
s=f[1],t=E(a,i[1]),u=E(a,s);return o(O[29],g[13],u,t)}}}else{var
j=b[2];if(j)if(!j[2]){var
v=E(a,j[1]);return c(O[16],g[10],v)}}else{var
k=b[2];if(k)if(!k[2]){var
w=E(a,k[1]);return c(O[16],g[22],w)}}else{var
l=b[2];if(l){var
m=l[2];if(m)if(!m[2]){var
x=l[1],y=E(a,m[1]),z=E(a,x);return o(O[29],g[14],z,y)}}}else{var
n=b[2];if(n){var
p=n[2];if(p)if(!p[2]){var
A=n[1],B=E(a,p[1]),C=E(a,A);return o(O[29],g[12],C,B)}}}}return 0}var
e=[0,cW,cY,c0,c2,c4,c6,c8,c_,da,dc,de,dg,di,dk,dm,dp,dr,dt,dv,dx,dz,dB,dD,dF,dH,dJ,dL,dN,dP,dR,dT,dV,dX,dZ,d1,d3,d5,d7,d9,d$,eb,bo,eD,en,el,[0,$,bv,eF,eH,eJ,eL,bu,eS,e3,E]];aP(228,e,"Romega_plugin.Const_omega");var
v=b(fh[1][2],[0,g[17],g[16],g[12],g[13],g[14],g[15],g[22],g[5],g[6],g[2]]),fi=0,fj=0,fk=x[1],fl=[0,function(b,a){return b===a?1:0},fk],z=b(fm[25],fl),S=[0,0],bw=T[66][2];function
fn(a){b(l[33],a);b(l[36],0);return b(l[52],l[28])}var
U=T[66][3];function
bx(c,a){switch(c){case
0:var
b=0===a?1:0;break;case
1:var
b=1===a?1:0;break;default:var
b=2<=a?1:0}return b?1:0}function
by(l,k){var
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
j=by(e,h);if(j){var
b=f,a=i;continue}return j}return 0}}function
aA(c,a){if(0===c[0]){var
f=c[1];if(0===a[0])var
e=a[1],d=f,b=1;else
var
b=0}else{var
g=c[1];if(0===a[0])var
b=0;else
var
e=a[1],d=g,b=1}return b?d===e?1:0:0}var
ab=b(t[1][6],fo);function
bz(d){var
a=b(z[1],7),c=b(z[1],7);return[0,0,0,b(z[1],7),0,c,a]}function
bA(a){a[4]=a[4]+1|0;return a[4]}function
bB(a){return 0===a[0]?[1,a[1]]:[0,a[1]]}function
fp(a){return a[1]}function
bC(d){function
a(f,e,d){if(d){var
h=d[2],j=d[1],g=c(fq[6],0,0),k=g[2],l=g[1],m=o(i[4],fr,f,e),n=a(f,e+1|0,h),p=b(s[5],0),q=o(fs[15],k,l,j),r=b(s[3],ft),t=b(s[3],m),u=b(s[13],0),v=c(s[12],u,t),w=c(s[12],v,r),x=c(s[12],w,q),y=c(s[12],x,p);return c(s[12],y,n)}return b(s[3],fu)}var
e=a(80,0,d[2]),f=b(s[5],0),g=b(s[3],fv),h=c(s[12],g,f),j=c(s[12],h,e),k=a(86,0,d[1]),l=b(s[5],0),m=b(s[3],fw),n=c(s[12],m,l),p=c(s[12],n,k),q=b(s[5],0),r=c(s[12],j,q),t=c(s[12],r,p);return c(fx[10],0,t)}var
aY=[0,-1];function
bD(a){aY[1]=-1;return 0}function
aZ(a){aY[1]++;return aY[1]}var
aB=[0,-1];function
bE(a){aB[1]=a;return 0}function
bF(a){aB[1]=-1;return 0}function
bG(a){aB[1]++;return aB[1]}function
aC(a){return c(i[4],fy,a)}function
a0(d,e,a){try{var
h=a[1],i=b(k[95],d),j=o(f[17][84],i,e,h);return j}catch(d){d=u(d);if(d===H){var
g=b(f[17][1],a[1]);a[1]=c(f[18],a[1],[0,e,0]);return g}throw d}}function
a1(a){try{var
c=b(f[17][7],a[1]);return c}catch(a){a=u(a);if(a[1]===bH)return b(l[3],fz);throw a}}function
bI(e,d,a){if(e===b(f[17][1],a[1])){a[1]=c(f[18],a[1],[0,d,0]);return 0}throw[0,ac,fA]}function
bJ(d,a,e){try{var
h=a[2],i=b(k[95],d),j=o(f[17][84],i,e,h);return j}catch(d){d=u(d);if(d===H){var
g=b(f[17][1],a[2]);a[2]=c(f[18],a[2],[0,e,0]);return g}throw d}}function
fB(d,a){try{var
e=c(f[17][7],d,a);return e}catch(a){a=u(a);if(a[1]===bH)return b(l[3],fC);throw a}}function
bK(b,a){var
d=a[7][1];return c(z[11],b[5],d)?0:o(z[5],b[5],d,a)}function
a2(a,b){try{var
d=c(z[7],a[5],b);return d}catch(a){a=u(a);if(a===H){c(i[2],fD,b);throw a}throw a}}function
C(c,a){switch(a[0]){case
0:var
d=b(g[2],a[1]);return o(i[1],c,fE,d);case
1:return Y(i[1],c,fF,C,a[1],C,a[2]);case
2:return Y(i[1],c,fG,C,a[1],C,a[2]);case
3:return Y(i[1],c,fH,C,a[1],C,a[2]);case
4:return aO(i[1],c,fI,C,a[1]);default:return o(i[1],c,fJ,a[1])}}function
bL(a){switch(a){case
0:return fK;case
1:return fL;case
2:return fM;case
3:return fN;case
4:return fO;default:return fP}}function
A(b,a){if(typeof
a==="number")return 0===a?c(i[1],b,fQ):c(i[1],b,fR);else
switch(a[0]){case
0:var
d=a[2],e=d[3],f=d[2],g=bL(d[1]);return ca(i[1],b,fS,C,f,g,C,e);case
1:return aO(i[1],b,fT,A,a[1]);case
2:return Y(i[1],b,fU,A,a[2],A,a[3]);case
3:return Y(i[1],b,fV,A,a[2],A,a[3]);case
4:return Y(i[1],b,fW,A,a[2],A,a[3]);default:return c(i[1],b,fX)}}function
bM(b){function
c(a){if(a){var
d=a[1],e=d[2],f=d[1];return[1,[2,[5,e],[0,f]],c(a[2])]}return[0,b[4]]}return c(b[3])}function
n(a,e){var
c=q(a),f=r===c?a[1]:d===c?b(p[2],a):a;return b(k[21],[0,f,e])}function
bN(d,a){function
c(a){switch(a[0]){case
0:return b(e[46][7],a[1]);case
1:var
f=a[1],g=c(a[2]),h=[0,c(f),g];return n(e[46][3],h);case
2:var
i=a[1],j=c(a[2]),k=[0,c(i),j];return n(e[46][4],k);case
3:var
l=a[1],m=c(a[2]),o=[0,c(l),m];return n(e[46][6],o);case
4:var
p=[0,c(a[1])];return n(e[46][5],p);default:var
q=a[1];return b(a1(d),q)}}return c(a)}function
bO(d,b){try{var
a=c(z[7],d[3],b);return a}catch(a){a=u(a);if(a===H){c(i[2],fY,b);var
e=d[3],f=function(b,a){return o(i[2],fZ,b,a)};c(z[12],f,e);throw H}throw a}}function
bP(a){switch(a[0]){case
1:var
b=e[9];return function(a){return n(b,a)};case
2:var
c=e[10];return function(a){return n(c,a)};case
3:var
d=e[12];return function(a){return n(d,a)};default:throw[0,ac,f0]}}function
aD(c,a){switch(a[0]){case
0:var
d=[0,b(e[46][7],a[1])];return n(e[8],d);case
4:var
i=[0,aD(c,a[1])];return n(e[11],i);case
5:var
j=bO(c,a[1]),k=[0,b(e[43],j)];return n(e[13],k);default:var
f=a[1],g=aD(c,a[2]),h=[0,aD(c,f),g];return b(bP(a),h)}}function
ad(a,b){try{var
c=aD(a,b);return c}catch(a){a=u(a);C(l[29],b);throw a}}function
bQ(a){switch(a){case
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
bR(a){if(typeof
a!=="number")switch(a[0]){case
2:var
b=e[24];return function(a){return n(b,a)};case
3:var
c=e[25];return function(a){return n(c,a)};case
4:var
d=e[26];return function(a){return n(d,a)}}throw[0,ac,f1]}function
ae(f,c,a){if(typeof
a==="number"){if(0===a){var
g=e[21],j=q(g);return r===j?g[1]:d===j?b(p[2],g):g}var
h=e[22],k=q(h);return r===k?h[1]:d===k?b(p[2],h):h}else
switch(a[0]){case
0:var
i=a[2],l=i[2],m=i[1],o=ad(c,i[3]),s=[0,ad(c,l),o];return b(bQ(m),s);case
1:var
t=[0,ae(f,c,a[1])];return n(e[23],t);case
5:var
x=bJ(f,c,a[1]),y=[0,b(e[42],x)];return n(e[27],y);default:var
u=a[2],v=ae(f,c,a[3]),w=[0,ae(f,c,u),v];return b(bR(a),w)}}function
aE(c,a,b){try{var
d=ae(c,a,b);return d}catch(a){a=u(a);A(l[29],b);throw a}}function
bS(b,a){var
c=a[1],d=ad(b,a[2]),f=[0,ad(b,c),d];return n(e[15],f)}var
B=x[2][7];function
I(d){var
a=d;for(;;)switch(a[0]){case
0:return x[2][1];case
1:var
e=a[1],f=I(a[2]);return c(B,I(e),f);case
2:var
g=a[1],h=I(a[2]);return c(B,I(g),h);case
3:var
i=a[1],j=I(a[2]);return c(B,I(i),j);case
4:var
a=a[1];continue;default:return b(x[2][5],a[1])}}function
aF(a){if(a){var
b=a[1],d=aF(a[2]),e=c(B,I(b[3]),d);return c(B,I(b[2]),e)}return x[2][1]}function
K(b){var
a=b;for(;;){if(typeof
a!=="number")switch(a[0]){case
0:return aF([0,a[2],0]);case
1:var
a=a[1];continue;case
2:var
d=a[2],e=K(a[3]);return c(B,K(d),e);case
3:var
f=a[2],g=K(a[3]);return c(B,K(f),g);case
4:var
h=a[2],i=K(a[3]);return c(B,K(h),i)}return x[2][1]}}function
a3(b,a){var
d=a[1],e=c(v[8],a[2],b);function
g(a){var
d=a[1];return[0,d,c(v[8],a[2],b)]}return[0,c(f[17][69],g,d),e]}function
bT(b,a){function
d(p,o){var
b=p,a=o;for(;;){if(b){if(a){var
f=a[2],j=a[1],k=j[2],h=j[1],i=b[2],l=b[1],m=l[2],e=l[1];if(e===h){var
n=c(v[6],m,k);if(c(g[17],n,g[5])){var
b=i,a=f;continue}return[0,[0,e,n],d(i,f)]}return c(v[19],e,h)?[0,[0,e,m],d(i,a)]:[0,[0,h,k],d(b,f)]}return b}return a}}var
e=c(v[6],b[2],a[2]);return[0,d(b[1],a[1]),e]}function
V(h){var
a=h;for(;;)switch(a[0]){case
0:return[0,0,a[1]];case
1:var
i=a[1],j=V(a[2]);return bT(V(i),j);case
2:var
b=a[1],e=a[2];if(0===e[0])var
d=e[1],f=b;else{if(0!==b[0])throw[0,ac,f2];var
d=b[1],f=a[2]}return c(g[17],d,g[5])?[0,0,v[11]]:a3(d,V(f));case
3:var
a=[1,a[1],[4,a[2]]];continue;case
4:var
k=V(a[1]);return a3(v[14],k);default:return[0,[0,[0,a[1],g[6]],0],g[5]]}}function
bU(i,b,a){var
d=a[2],e=a[1];function
g(a){return[0,a[2],a[1]]}var
h=c(f[17][69],g,e);return[0,aZ(0),b,h,d]}function
bV(a){switch(a){case
0:return 5;case
1:return 3;case
2:return 4;case
3:return 1;case
4:return 2;default:return 0}}function
bW(j,g,h,c,a){var
i=g[1],k=g[4],l=g[3],m=g[2];function
d(e,d){var
g=bU(j,d,V(e));return[0,h,c,a,[0,l,b(f[17][9],k)],i,m,g]}try{var
n=i?bV(h):h;switch(n){case
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
e=d([1,c,[4,a]],2)}return e}catch(a){a=u(a);if(b(f3[5],a))throw a;throw a}}function
bX(c,b,a){return[2,c,b,a]}function
a4(c,b,a){return[3,c,b,a]}function
bY(c,b,a){return[4,c,b,a]}function
a5(b,a,f,e,d){var
g=J(b,a,e);return c(f,g,J(b,a,d))}function
J(a,d,f){var
b=c(e[46][8],a,f);if(typeof
b==="number")return[5,a0(a,f,d)];else
switch(b[0]){case
0:var
k=b[2],l=b[1];return a5(a,d,function(b,a){return[1,b,a]},l,k);case
1:var
g=b[2],h=b[1],i=c(e[46][10],a,h);if(i){var
m=i[1];return[2,[0,m],J(a,d,g)]}var
j=c(e[46][10],a,g);if(j){var
n=[0,j[1]];return[2,J(a,d,h),n]}return[5,a0(a,f,d)];case
2:var
o=b[2],p=b[1];return a5(a,d,function(b,a){return[3,b,a]},p,o);case
3:return[4,J(a,d,b[1])];case
4:var
q=[0,v[12]];return[1,J(a,d,b[1]),q];default:return[0,b[1]]}}function
af(k,c,a,g,p,j,n,m,l){var
h=a[4],i=a[3],d=a[2],q=a[1],e=bA(c),r=g?[0,[0,e],d]:d,s=g?[0,[1,e],d]:d;if(g){var
t=[0,i,b(f[17][9],h)];o(z[5],c[6],e,t)}var
u=W(k,c,[0,p,r,i,[0,0,h]],j,m);return o(n,e,u,W(k,c,[0,q,s,i,[0,1,h]],j,l))}function
L(b,a,h,g,f,e,d){var
i=J(b,a,e),c=bW(a,h,f,i,J(b,a,d));bK(a,c);return[0,g,c]}function
W(f,d,b,i,h){var
g=b[1],m=b[4],n=b[3],o=b[2],a=c(e[46][9],i,h);if(typeof
a==="number")switch(a){case
0:return 0;case
1:return 1;default:return[5,h]}else
switch(a[0]){case
0:return L(f,d,b,h,0,a[1],a[2]);case
1:return L(f,d,b,h,5,a[1],a[2]);case
2:return L(f,d,b,h,4,a[1],a[2]);case
3:return L(f,d,b,h,1,a[1],a[2]);case
4:return L(f,d,b,h,3,a[1],a[2]);case
5:return L(f,d,b,h,2,a[1],a[2]);case
6:return[1,W(f,d,[0,1-g,o,n,[0,2,m]],i,a[1])];case
7:return af(f,d,b,1-g,g,i,bX,a[1],a[2]);case
8:return af(f,d,b,g,g,i,a4,a[1],a[2]);case
9:return af(f,d,b,1-g,1-g,i,bY,a[1],a[2]);default:var
j=a[2],l=a[1],p=c(k[33],j,l);return af(f,d,b,g,g,i,a4,c(k[33],l,j),p)}}function
bZ(e,d,a){b(i[2],f4);o(i[2],f5,A,d);function
g(a){var
c=a[3],d=b(t[1][8],a[1]);return aO(i[2],f6,d,A,c)}c(f[17][11],g,a);return bC(e)}function
b0(j,i,h,a){var
l=[0,0,0,a,0],f=c(az[42][15],a,h);if(0===f[0])var
m=f[2];else{var
n=f[3],s=f[2];if(c(e[46][2],h,n)){var
g=e[5],o=q(g),t=r===o?g[1]:d===o?b(p[2],g):g;return[0,a,0,L(j,i,l,t,0,b(k[10],a),s)]}var
m=n}return[0,a,1,W(j,i,l,h,m)]}function
b1(d,a){var
e=b(aa[67][5],a),i=b(az[42][6],a),j=b(az[42][12],a),g=W(e,d,[0,1,0,ab,f7],a,i);function
k(b){return b0(e,d,a,b)}var
h=c(f[17][69],k,j);if(S[1])bZ(d,g,h);return[0,g,h]}function
bf(e,b,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,[0,a[2],b],0];case
1:var
d=a[1];return e<50?be(e+1|0,b,d):cb(be,[0,b,d]);case
2:var
g=a[3],h=M(b,a[2]),i=M(b,g);return c(f[18],h,i);case
3:var
j=a[3],k=M(b,a[2]),l=function(a){return M(a,j)};return c(f[17][78],l,k);case
4:var
m=a[3],n=P(b,a[2]),o=M(b,m);return c(f[18],n,o)}return[0,b,0]}function
be(e,b,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,[0,a[2],b],0];case
1:var
d=a[1];return e<50?bf(e+1|0,b,d):cb(bf,[0,b,d]);case
2:var
g=a[3],h=P(b,a[2]),i=function(a){return P(a,g)};return c(f[17][78],i,h);case
3:var
j=a[3],k=P(b,a[2]),l=P(b,j);return c(f[18],k,l);case
4:var
m=a[3],n=M(b,a[2]),o=function(a){return P(a,m)};return c(f[17][78],o,n)}return[0,b,0]}function
M(a,b){return cc(bf(0,a,b))}function
P(a,b){return cc(be(0,a,b))}function
a6(a){if(a){var
b=a[2],c=M(0,a[1][3]),d=a6(b);return o(f[17][143],f[18],c,d)}return f8}function
aG(a){return 0===a[0]?c(i[2],f9,a[1]):c(i[2],f_,a[1])}function
b2(a){function
g(a){var
g=e[7],j=q(g),s=r===j?g[1]:d===j?b(p[2],g):g;A(l[28],[0,s,a]);b(l[36],0);var
h=a[7],k=b(v[31],h[2]),m=[0,h[3],h[4]];function
n(a){return b(v[29],aC)}bg(i[2],f$,h[1],n,m,k);b(i[2],ga);c(f[17][11],aG,a[6]);var
u=a[4][2];function
w(a){switch(a){case
0:return gb;case
1:return gc;default:return gd}}var
x=c(f[17][69],w,u),y=c(f[15][7],ge,x);c(i[2],gf,y);var
z=a[5]?gg:gi,B=b(t[1][8],a[4][1]);return o(i[2],gh,B,z)}function
h(a){b(i[2],gj);return c(f[17][11],g,a)}return c(f[17][11],h,a)}function
ag(e){var
a=e;for(;;){if(a){var
d=a[2],b=a[1];switch(b[0]){case
6:var
f=b[1],g=ag(d);return c(x[2][4],f[1],g);case
15:var
h=b[2][2],i=ag(b[3][2]);return c(B,ag(h),i);default:var
a=d;continue}}return x[2][1]}}function
b3(b,a){return cd(b[5],a[5])}var
X=b(f[20][1],[0,b3]);function
ah(d){var
a=d;for(;;){if(a){var
b=a[1];switch(b[0]){case
5:var
e=b[1],f=ah(a[2]);return c(X[4],e,f);case
15:if(!a[2]){var
g=b[2][2],h=ah(b[3][2]),i=ah(g);return c(X[7],i,h)}break}var
a=a[2];continue}return X[1]}}function
aH(a){if(0===a[0])return ah(a[1][3]);var
b=a[2],d=aH(a[3]),e=aH(b);return c(X[7],e,d)}function
a7(f){var
a=e[46][1],c=q(a),g=r===c?a[1]:d===c?b(p[2],a):a;return n(e[1],[0,g,f])}function
b4(c,d){function
e(a,b){var
f=b[4],g=b[3],h=b[2],i=b[1],d=bM(a[2]),e=bN(c,d);bI(a[5],e,c);var
j=a7(e);return[0,[0,a[5],i],[0,j,h],[0,[0,d,[5,a[5]]],g],[0,[1,a[2][1]],f]]}var
g=aH(d),a=o(X[15],e,g,gk),h=a[3],i=a[2],j=a[1],k=b(f[17][9],a[4]),l=b(f[17][9],h),m=b(f[17][9],i);return[0,b(f[17][9],j),m,l,k]}function
a8(c,a){if(a){var
e=a[2],g=a[1];try{var
j=a2(c,g)[6],d=j}catch(a){a=u(a);if(a!==H)throw a;var
d=0}var
h=a8(c,e),i=b(f[17][9],d);return o(f[17][132],aA,i,h)}return 0}function
b5(a){function
d(c,a){var
d=c[2],e=b(f[17][1],a[2]);return cd(b(f[17][1],d),e)}try{var
e=c(f[17][39],d,a),g=b(f[17][5],e);return g}catch(a){a=u(a);if(a[1]===gl)return b(l[3],gm);throw a}}function
b6(d,a){function
e(g){var
a=g;for(;;){if(a){var
c=a[2],b=a[1];if(o(f[17][49],aA,b,d)){var
a=c;continue}var
h=bB(b);if(o(f[17][49],aA,h,d))throw l[4];return[0,b,e(c)]}return 0}}function
b(a){var
b=a[2],c=a[1];try{var
d=[0,[0,c,e(b)]];return d}catch(a){a=u(a);if(a===l[4])return 0;throw a}}return c(f[17][66],b,a)}function
aI(a){if(0===a[0])return a[1][2];var
b=a[2],d=aI(a[3]);return c(B,aI(b),d)}function
b7(m,a){function
b(a){if(typeof
a==="number")return 0===a?[5,n(e[5],[0])]:[5,n(e[6],[0])];else
switch(a[0]){case
0:var
o=a[1];return c(x[2][3],a[2][7][1],m)?a:[5,o];case
1:var
d=b(a[1]);if(typeof
d!=="number"&&5===d[0])return[5,n(e[3],[0,d[1]])];return[1,d];case
2:var
p=a[3],q=a[1],f=b(a[2]),g=b(p);if(typeof
f!=="number"&&5===f[0])if(typeof
g!=="number"&&5===g[0])return[5,n(e[4],[0,f[1],g[1]])];return[2,q,f,g];case
3:var
r=a[3],s=a[1],h=b(a[2]),i=b(r);if(typeof
h!=="number"&&5===h[0])if(typeof
i!=="number"&&5===i[0])return[5,n(e[2],[0,h[1],i[1]])];return[3,s,h,i];case
4:var
t=a[3],u=a[1],j=b(a[2]),l=b(t);if(typeof
j!=="number"&&5===j[0])if(typeof
l!=="number"&&5===l[0])return[5,c(k[33],j[1],l[1])];return[4,u,j,l];default:return a}}return b(a)}function
aJ(d,a){if(0===a[0]){var
e=a[1],g=b(x[2][21],e[2]),h=c(f[17][69],l[22],g),j=c(f[15][7],gn,h),k=o(i[4],go,e[1],j);return c(l[55],d,k)}return ca(i[1],d,gp,a[1],aJ,a[2],aJ,a[3])}function
aK(a,d){function
e(g,d,c){if(c){var
h=c[1];if(0===h[0]){var
i=h[1],k=c[2],l=aK(a,b(f[17][9],[0,[1,i],d]));return[1,i,e(g,[0,[0,i],d],k),l]}var
j=h[1],m=e(g,[0,[1,j],d],c[2]);return[1,j,aK(a,b(f[17][9],[0,[0,j],d])),m]}return[0,g]}var
g=b6(d,a);try{var
h=b5(g)}catch(e){e=u(e);var
j=b(f[17][1],a),k=b(f[17][1],g);o(i[2],gq,k,j);c(f[17][11],aG,d);throw e}var
l=h[2],m=h[1];return e(m,b(f[17][9],d),l)}function
a9(i,m){var
d=0,a=m,n=i[2],o=i[1];a:for(;;){if(a){var
j=a[1];if(0===j[0]){var
k=j[1],r=a[2],s=k[2];if(c(t[1][1],o,k[1])){var
e=[0,s,n];for(;;){var
f=e[1];if(f){var
g=e[2];if(g){var
p=g[2],q=f[2];if(bx(f[1],g[1])){var
e=[0,q,p];continue}}var
h=0}else
var
h=[0,e[2]];if(h)return[0,d,h[1]];var
d=d+1|0,a=r;continue a}}}var
d=d+1|0,a=a[2];continue}return b(l[3],gr)}}function
a_(h){function
i(h){switch(h){case
0:var
a=e[35],f=q(a),i=r===f?a[1]:d===f?b(p[2],a):a;return[0,i];case
1:var
c=e[36],g=q(c),j=r===g?c[1]:d===g?b(p[2],c):c;return[0,j];default:return 0}}var
j=c(f[17][66],i,h),a=e[34],g=q(a),k=r===g?a[1]:d===g?b(p[2],a):a;return c(e[44],k,j)}function
y(h,f){var
d=0,a=h;for(;;){if(a){var
g=a[1];if(1===g[0])if(f===g[1])return b(e[42],d);var
d=d+1|0,a=a[2];continue}var
j=c(i[4],gs,f);return b(l[3],j)}}function
a$(l,j,i){var
m=[0,b(e[42],0)],a=e[28],f=q(a),n=r===f?a[1]:d===f?b(p[2],a):a,o=b(k[21],[0,n,m]),s=l?v[14]:g[6],t=b(e[46][7],s),u=[0,b(e[46][7],g[6]),j,t,i,o],c=e[31],h=q(c),w=r===h?c[1]:d===h?b(p[2],c):c;return b(k[21],[0,w,u])}function
N(h,c,L){var
f=L;for(;;){if(f){var
a=f[1];switch(a[0]){case
0:var
z=f[2],x=a[3],w=a[1],i=1;break;case
1:if(!f[2]){var
R=a[1],S=b(e[46][7],a[2]),T=[0,y(c,R[1]),S],m=e[30],B=q(m),U=r===B?m[1]:d===B?b(p[2],m):m;return b(k[21],[0,U,T])}var
i=0;break;case
3:var
z=f[2],x=a[2],w=a[1],i=1;break;case
4:var
C=a[3],D=a[2],V=C[2],W=C[1],X=D[2],Y=D[1],Z=N(h,[0,[1,a[1]],c],f[2]),_=y(c,V[1]),$=b(e[46][7],W),aa=y(c,X[1]),ab=[0,b(e[46][7],Y),aa,$,_,Z],n=e[31],E=q(n),ac=r===E?n[1]:d===E?b(p[2],n):n;return b(k[21],[0,ac,ab]);case
5:var
o=a[1],ad=o[4],ae=o[3],af=o[2],ag=N(h,[0,[1,o[1][1]],c],f[2]),ah=y(c,af[1]),ai=b(e[46][7],ad),aj=y(c,ae[1]),ak=[0,b(e[46][7],g[6]),aj,ai,ah,ag],s=e[31],F=q(s),al=r===F?s[1]:d===F?b(p[2],s):s;return b(k[21],[0,al,ak]);case
6:var
f=f[2];continue;case
9:if(!f[2]){var
am=a[1],an=y(c,a[2][1]);return a$(0,y(c,am[1]),an)}var
i=0;break;case
10:if(!f[2]){var
ao=a[3],ap=a[1],aq=y(c,a[2][1]);return a$(ao,y(c,ap[1]),aq)}var
i=0;break;case
11:var
ar=a[3],as=a[2],at=N(h,[0,[1,a[1]],c],f[2]),au=y(c,ar),av=[0,y(c,as[1]),au,at],t=e[32],G=q(t),aw=r===G?t[1]:d===G?b(p[2],t):t;return b(k[21],[0,aw,av]);case
15:var
I=a[3],J=a[2],az=I[2],aA=I[1],aB=a[1],aC=N(h,[0,[1,J[1]],c],J[2]),aD=N(h,[0,[1,aA],c],az),aE=[0,y(c,aB[1]),aC,aD],v=e[33],K=q(v),aF=r===K?v[1]:d===K?b(p[2],v):v;return b(k[21],[0,aF,aE]);case
16:return b(l[3],gu);case
12:case
13:case
14:if(!f[2]){var
ax=[0,y(c,a[1])],u=e[28],H=q(u),ay=r===H?u[1]:d===H?b(p[2],u):u;return b(k[21],[0,ay,ax])}var
i=0;break;default:var
f=f[2];continue}if(i){var
M=N(h,c,z),O=b(e[46][7],x),P=[0,y(c,w[1]),O,M],j=e[29],A=q(j),Q=r===A?j[1]:d===A?b(p[2],j):j;return b(k[21],[0,Q,P])}}return b(l[3],gt)}}function
ba(h,f,d,a){if(a){var
j=a[1],m=a[2];try{var
v=c(z[7],f[5],j),g=v}catch(a){a=u(a);if(a!==H)throw a;var
o=c(i[4],gy,j),g=b(l[3],o)}var
k=a9(g[4],d),p=k[2],q=k[1],r=ba(h,f,[0,[1,g[7][1]],d],m),s=a_(p),t=[0,b(e[42],q),s,r];return n(e[38],t)}var
w=[0,N(f,d,h)];return n(e[39],w)}function
aL(h,g,d){if(0===d[0]){var
j=d[1],o=b(x[2][21],j[2]);return ba(j[3],h,g,o)}var
k=d[1],p=d[3],q=d[2];try{var
E=c(z[7],h[6],k),a=E}catch(d){d=u(d);if(d!==H)throw d;var
r=c(i[4],gv,k),a=b(l[3],r)}var
m=a9(a,g),s=m[2],t=m[1],v=c(f[18],a[2],gw),w=[0,[0,a[1],v]],y=c(f[18],a[2],gx),A=aL(h,[0,[0,[0,a[1],y]],g],p),B=aL(h,[0,w,g],q),C=a_(s),D=[0,b(e[42],t),C,B,A];return n(e[37],D)}function
b8(j,e,h){function
k(a){return a[7]}var
m=c(f[17][69],k,h),a=c(v[68],[0,aZ,bG,aC],m),d=ag(a),g=a8(j,b(x[2][21],d));if(S[1]){c(i[2],gz,e);c(v[36],aC,a);b(l[31],gA);var
n=function(a){return c(i[2],gB,a)};c(x[2][13],n,d);b(l[31],gC);c(f[17][11],aG,g);b(i[2],gD)}return[0,[0,e,d,a],g]}function
b9(R,w,a,y,P){var
A=y[1],T=y[2];if(S[1])b(i[2],gE);function
V(b,c){return b8(a,b,c)}var
g=aK(c(f[17][13],V,P),0);if(S[1]){aJ(l[28],g);b(l[36],0)}var
C=aI(g),W=K(A),X=[0,t[1][10][1],W];function
Y(e,b){var
f=b[2],g=b[1],d=a2(a,e),h=c(B,aF([0,d,0]),f);return[0,c(t[1][10][4],d[4][1],g),h]}var
D=o(x[2][15],Y,C,X),Z=D[2],_=c(t[1][10][6],ab,D[1]),E=b(t[1][10][21],_),h=b4(a,g),$=h[4],aa=h[3],ad=h[2],ae=h[1],af=b(x[2][21],Z),ag=c(f[18],af,ae);function
F(d,c){if(c){var
e=c[1],f=c[2],g=b(a1(a),e);o(z[5],a[3],e,d);return[0,g,F(d+1|0,f)]}return 0}var
ah=F(0,ag),j=e[46][1],G=q(j),ai=r===G?j[1]:d===G?b(p[2],j):j,aj=c(e[44],ai,ah);function
ak(b){return bS(a,b)}var
al=c(f[17][69],ak,aa),am=aE(w,a,A);function
an(d){try{var
e=c(t[1][11][22],d,T)}catch(a){a=u(a);if(a===H)throw[0,ac,gF];throw a}if(0===e[1]){var
f=e[2],g=a7(b(k[10],d));return[0,aE(w,a,f),g]}var
h=e[2],i=b(k[10],d);return[0,aE(w,a,b7(C,h)),i]}var
ao=c(f[17][69],an,E),I=b(f[17][123],ao),ap=I[2],aq=I[1],ar=b(e[45],a[2]),as=c(f[18],al,aq),m=e[14],J=q(m),at=r===J?m[1]:d===J?b(p[2],m):m,au=[0,am,ar,aj,c(e[44],at,as)],av=n(e[40],au);function
L(a){return[0,a,0]}function
aw(a){return[0,L(a)]}var
ax=c(f[17][69],aw,E),ay=[0,[0,L(ab)],$],az=aL(a,c(f[18],ay,ax),g),s=e[7],M=q(s),aA=r===M?s[1]:d===M?b(p[2],s):s,aB=b(Q[86],aA);if(R)var
v=e[5],N=q(v),aC=0,aD=r===N?v[1]:d===N?b(p[2],v):v,O=c(Q[5],aD,aC);else
var
O=Q[67];var
aG=n(e[41],[0,az]),aH=b(Q[86],aG),aM=c(Q[5],av,2),aN=c(f[18],ad,ap);return c(U,c(U,c(U,c(U,c(U,b(Q[146],aN),aM),aH),bw),O),aB)}var
bb=[0,v,fi,fj,z,S,bw,fn,U,bx,by,aA,ab,bz,bA,bB,fp,bC,aZ,bD,bG,bF,bE,aC,a0,a1,bI,bJ,fB,bK,a2,C,bL,A,bM,n,bN,bO,bP,ad,bQ,bR,ae,aE,bS,B,I,aF,K,a3,bT,V,bU,bV,bW,bX,a4,bY,J,a5,af,L,W,bZ,b0,b1,M,P,a6,aG,b2,ag,b3,X,ah,aH,a7,b4,a8,b5,b6,aI,b7,aJ,aK,a9,a_,y,a$,N,aL,ba,b8,b9,function(i){function
a(c){b(D[3],gG);bD(0);bF(0);try{var
a=bz(0),d=b1(a,c),e=d[2],g=d[1];bE(b(f[17][1],a[1])-1|0);var
h=a6([0,[0,ab,1,[1,g]],e]),k=t[1][11][1],l=function(b,a){return o(t[1][11][4],a[1],[0,a[2],a[3]],b)},m=o(f[17][15],l,k,e);if(S[1])b2(h);var
n=b9(i,b(aa[67][5],c),a,[0,g,m],h);return n}catch(a){a=u(a);if(a===v[28]){var
j=b(s[3],gH);return o(b_[6],0,0,j)}throw a}}return b(aa[67][8],a)}];aP(245,bb,"Romega_plugin.Refl_omega");b(gI[10],aM);function
ai(a){var
d=c(bc[17],t[1][6],gJ),e=b(t[5][4],d),f=b(t[6][4],a),g=c(t[13][2],[0,e],f),h=b(aN[4][12],g);return b(aN[13][22],h)}function
aj(d,a){var
e=c(f[17][141],gK[33],a);function
g(a){if(h(a,gL)){if(h(a,gM)){if(h(a,gN)){if(h(a,gO)){var
d=c(l[17],gP,a),e=b(s[3],d);return o(b_[6],0,0,e)}return ai(gQ)}return ai(gR)}return ai(gS)}return ai(gT)}var
i=c(bc[17],g,e),j=b(bb[94],d),k=c(T[66][3],Q[28],j),m=b(T[66][22],i),n=b(aa[60],m),p=b(T[66][32],n);return c(T[66][3],p,k)}var
bd=o(gW[1],gV,gU,0),gX=0,gZ=[0,[0,gY,function(a){return aj(1,0)}],gX],g1=[0,[0,g0,function(a){return aj(0,0)}],gZ];bg(aN[10][8],aM,g2,0,[0,bd],g1);var
g3=0,g6=[0,[0,g5,function(a){return aj(0,g4)}],g3];function
g7(a,b){return aj(0,c(bc[17],t[1][8],a))}var
hb=[0,[0,[0,ha,[0,g$,[1,[0,[5,b(g_[16],g9[7])]],g8,0]]],g7],g6];bg(aN[10][8],aM,hc,0,[0,bd],hb);var
b$=[0,aM,ai,aj,bd];aP(253,b$,"Romega_plugin.G_romega");aP(254,[0,e,bb,b$],"Romega_plugin");return}
