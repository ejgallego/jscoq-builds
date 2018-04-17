function(hd){"use strict";var
cf="xO",ck=" - ",cl="not",r=250,cj="Z.succ",cv="Z.pred",bj="Z.opp",cn="xH",aU="Zpos",cu="REIFED PROBLEM\n\n",d=246,ci="FF",bg="Z.sub",cq="True",aS="Z",_="romega",al="plugins/romega/refl_omega.ml",ct="\n====================================\n",bi="N",cy="False",cp="with",bh="Z.mul",cx="Prop",aT="",cm="or",cs="TT",aV="Omega",ce="=SYSTEM===================================\n",aR="Zneg",bf="Z.add",ch="positive",aQ="Z0",cr="  Depends on:",cw="nat",co="and",bk=121,cg="\n------------------------------------\n",cd="xI",R="Coq",w=hd.jsoo_runtime,cc=w.caml_int_compare,a=w.caml_new_string,q=w.caml_obj_tag,aP=w.caml_register_global,h=w.caml_string_notequal,cb=w.caml_trampoline,ca=w.caml_trampoline_return,u=w.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):w.caml_call_gen(a,[b])}function
c(a,b,c){return a.length==2?a(b,c):w.caml_call_gen(a,[b,c])}function
p(a,b,c,d){return a.length==3?a(b,c,d):w.caml_call_gen(a,[b,c,d])}function
Y(a,b,c,d,e){return a.length==4?a(b,c,d,e):w.caml_call_gen(a,[b,c,d,e])}function
hc(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):w.caml_call_gen(a,[b,c,d,e,f])}function
Z(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):w.caml_call_gen(a,[b,c,d,e,f,g])}function
b$(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):w.caml_call_gen(a,[b,c,d,e,f,g,h])}var
j=w.caml_get_global_data(),aN=a("romega_plugin"),o=j.CamlinternalLazy,k=j.EConstr,g=j.Bigint,O=j.Option,ab=j.Proofview,aA=j.Tacmach,s=j.Names,bp=j.Univ,D=j.Coqlib,$=j.Universes,aX=j.Nametab,f=j.Util,l=j.Pervasives,i=j.Printf,x=j.Int,ad=j.Assert_failure,H=j.Not_found,t=j.Pp,b9=j.CErrors,Q=j.Tactics,bG=j.Invalid_argument,T=j.Tacticals,bc=j.List,aO=j.Ltac_plugin,bl=[0,a(R),[0,a(_),[0,a("ReflOmegaCore"),0]]],e9=a(bf),e_=a(bh),e$=a(bj),fa=a(cv),fb=a(bg),fc=a(cj),fd=a(aQ),fe=a(aR),ff=a(aU),e3=a("Z.ge"),e4=a("Z.gt"),e5=a("Z.le"),e6=a("Z.lt"),e7=a("Zne"),e8=a("eq"),eS=a(bf),eT=a(bh),eU=a(bj),eV=a(cv),eW=a(bg),eX=a(cj),eY=a(aQ),eZ=a(aR),e0=a(aU),eO=a(aQ),eP=a(aR),eQ=a(aU),eL=a(cn),eM=a(cd),eN=a(cf),eJ=a(bg),eH=a(bj),eF=a(bh),eD=a(bf),eB=a("Npos"),eA=a("N0"),ez=a(aR),ey=a(aU),ex=a(aQ),ew=a(cd),ev=a(cf),eu=a(cn),et=a(aS),en=a(cy),eo=a(cq),ep=a(co),eq=a("iff"),er=a(cl),es=a(cm),ei=a("nil"),eh=a("cons"),ed=[0,a("Init"),[0,a("Datatypes"),0]],ee=a(aT),ec=a("O"),eb=a("S"),d$=a("do_omega"),d9=a("interp_goal_concl"),d7=a("E_SOLVE"),d5=a("E_EXTRACT"),d3=a("E_SPLIT"),d1=a("D_right"),dZ=a("D_left"),dX=a("direction"),dV=a("O_SPLIT_INEQ"),dT=a("O_MERGE_EQ"),dR=a("O_SUM"),dP=a("O_NOT_EXACT_DIVIDE"),dN=a("O_DIVIDE"),dL=a("O_BAD_CONSTANT"),dJ=a("Tprop"),dH=a("Timp"),dF=a("Tand"),dD=a("Tor"),dB=a("Tnot"),dz=a("FalseTerm"),dx=a("TrueTerm"),dv=a("NeqTerm"),dt=a("GtTerm"),dr=a("LtTerm"),dp=a("GeqTerm"),dm=a("LeqTerm"),dk=a("EqTerm"),di=a("proposition"),dg=a("Tvar"),de=a("Tminus"),dc=a("Topp"),da=a("Tmult"),c_=a("Tplus"),c8=a("Tint"),c6=a("I"),c4=a(cy),c2=a(cq),c0=a(cm),cY=a(cl),cW=a(co),cU=a("eq_refl"),cT=a(aV),cS=a(aV),cR=a(aV),cQ=a(aV),cA=a("."),cB=a(aT),cC=a(aT),cz=[0,a(aS),[0,a(bi),[0,a("Pos"),0]]],cD=a("Const_omega.DestConstApp"),cE=[0,a(R),[0,a("Logic"),[0,a("Decidable"),0]]],cF=[0,a("ZOmega"),0],cI=[0,[0,a(R),[0,a("Lists"),[0,a("List"),0]]],0],cO=[0,[0,a(R),[0,a("Numbers"),[0,a("BinNums"),0]]],0],cP=[0,[0,a(R),[0,a("ZArith"),[0,a("BinInt"),0]]],0],fD=[0,[2,0,0],a("%s")],fE=[0,[12,40,[15,[11,a(" + "),[15,[12,41,0]]]]],a("(%a + %a)")],fF=[0,[12,40,[15,[11,a(" * "),[15,[12,41,0]]]]],a("(%a * %a)")],fG=[0,[12,40,[15,[11,a(ck),[15,[12,41,0]]]]],a("(%a - %a)")],fH=[0,[11,a("~ "),[15,0]],a("~ %a")],fI=[0,[12,86,[4,0,[0,2,2],0,0]],a("V%02d")],fP=[0,[11,a(cs),0],a(cs)],fQ=[0,[11,a(ci),0],a(ci)],fR=[0,[15,[12,32,[2,0,[12,32,[15,0]]]]],a("%a %s %a")],fS=[0,[11,a("not("),[15,[12,41,0]]],a("not(%a)")],fT=[0,[12,40,[15,[11,a(" or "),[15,[12,41,0]]]]],a("(%a or %a)")],fU=[0,[12,40,[15,[11,a(" and "),[15,[12,41,0]]]]],a("(%a and %a)")],fV=[0,[12,40,[15,[11,a(" => "),[15,[12,41,0]]]]],a("(%a => %a)")],fW=[0,[11,a(cx),0],a(cx)],f1=[0,a(al),439,15],f7=[0,0,0],gm=a(" "),gn=[0,[4,0,0,0,[12,91,[2,0,[12,93,0]]]],a("%d[%s]")],go=[0,[12,83,[4,0,0,0,[12,40,[15,[12,44,[15,[12,41,0]]]]]]],a("S%d(%a,%a)")],gp=[0,[4,0,0,0,[11,a(ck),[4,0,0,0,[12,10,0]]]],a("%d - %d\n")],gt=a("not_treated"),gs=a("bad history"),gu=[0,[11,a("Cannot find constructor "),[4,0,0,0,0]],a("Cannot find constructor %d")],gv=[0,0,0],gw=[0,1,0],gx=[0,[11,a("Cannot find equation "),[4,0,0,0,0]],a("Cannot find equation %d")],gF=[0,a(R),[0,a(_),[0,a("ROmega"),0]]],gG=a("ROmega can't solve this system"),gE=[0,a(al),1019,33],gD=[0,[11,a(ct),0],a(ct)],gA=[0,[12,32,[4,0,0,0,0]],a(" %d")],gy=[0,[11,a("SYSTEME "),[4,0,0,0,[12,10,0]]],a("SYSTEME %d\n")],gz=a("\n  Depend :"),gB=a("\n  Split points :"),gC=[0,[11,a(cg),0],a(cg)],gr=[0,[11,a("get_hyp "),[4,0,0,0,0]],a("get_hyp %d")],gq=a("find_path"),gl=a("select_smaller"),gj=[0,0,0,0,0],gi=[0,[11,a(ce),0],a(ce)],ga=a("L"),gb=a("R"),gc=a("M"),f$=[0,[11,a(cr),0],a(cr)],gd=a(aT),ge=[0,[11,a("\n  Path: "),[2,0,0]],a("\n  Path: %s")],gf=a("yes"),gh=a("no"),gg=[0,[11,a("\n  Origin: "),[2,0,[11,a(" (negated : "),[2,0,[11,a(")\n\n"),0]]]]],a("\n  Origin: %s (negated : %s)\n\n")],f_=[0,[11,a("  E"),[4,0,0,0,[11,a(" : "),[15,[12,32,[2,0,[11,a(" 0\n"),0]]]]]]],a("  E%d : %a %s 0\n")],f8=[0,[11,a(" L"),[4,0,0,0,0]],a(" L%d")],f9=[0,[11,a(" R"),[4,0,0,0,0]],a(" R%d")],f6=[0,2,0],f5=[0,[11,a("  "),[2,0,[11,a(": "),[15,[12,10,0]]]]],a("  %s: %a\n")],f3=[0,[11,a(cu),0],a(cu)],f4=[0,[11,a("  CONCL: "),[15,[12,10,0]]],a("  CONCL: %a\n")],f0=[0,a(al),347,9],fZ=[0,a(al),322,9],fY=[0,[4,0,0,0,[11,a(" -> "),[4,0,0,0,[12,10,0]]]],a("%d -> %d\n")],fX=[0,[11,a("Atome "),[4,0,0,0,[11,a(" non trouv\xc3\xa9\n"),0]]],a("Atome %d non trouv\xc3\xa9\n")],fJ=a("="),fK=a("<="),fL=a(">="),fM=a(">"),fN=a("<"),fO=a("!="),fC=[0,[11,a("Omega Equation "),[4,0,0,0,[11,a(" non trouv\xc3\xa9e\n"),0]]],a("Omega Equation %d non trouv\xc3\xa9e\n")],fB=a("get_prop"),fz=[0,a(al),233,2],fy=a("get_reified_atom"),fx=[0,[11,a("OV"),[4,0,0,0,0]],a("OV%d")],fq=[0,[12,40,[0,[4,0,[0,2,2],0,[12,41,0]]]],a("(%c%02d)")],fs=a(" := "),ft=a("  ===============================\n\n"),fu=a("ENVIRONMENT OF PROPOSITIONS :"),fv=a("ENVIRONMENT OF TERMS :"),fn=a("__goal__"),g0=[0,a(cw),[0,a(ch),[0,a(bi),[0,a(aS),0]]]],gK=a(bi),gL=a(aS),gM=a(cw),gN=a(ch),gP=a("zify_positive"),gQ=a("zify_nat"),gR=a("zify_op"),gS=a("zify_N"),gO=a("No ROmega knowledge base for type "),gI=[0,a("PreOmega"),[0,a("omega"),[0,a(R),0]]],gU=[0,a("unsafe_romega"),0],gW=[0,a(_),0],gY=a(_),g1=[0,a(_),[0,a(cp),[0,a("*"),0]]],g4=a("$l"),g_=a(cp),g$=a(_),hb=a("romega'"),e1=j.Reductionops,eg=j.Globnames,ef=j.Global,gk=j.Failure,f2=j.Logic,fp=j.Pfedit,fr=j.Printer,fw=j.Feedback,fg=j.Omega_plugin,fl=j.Hashtbl,gJ=j.String,gH=j.Mltop,g6=j.Stdarg,g7=j.Genarg,g9=j.Loc;function
aW(a){var
h=b(aX[40],a),d=b(s[5][5],h);if(d)var
e=b(s[1][8],d[1]),i=c(f[15][50][2],e,cz)?c(l[16],e,cA):cB,g=i;else
var
g=cC;var
j=b(aX[41],a),k=b(s[1][8],j);return c(l[16],g,k)}function
am(e,g){var
f=c(k[82],e,g),d=f[2],a=c(k[3],e,f[1]);switch(a[0]){case
1:if(!d)return[0,b(s[1][8],a[1])];break;case
6:if(!a[1])if(!d)return[2,a[2],a[3]];break;case
10:return[1,aW([1,a[1][1]]),d];case
11:return[1,aW([2,a[1][1]]),d];case
12:return[1,aW([3,a[1][1]]),d]}return 0}var
an=[248,cD,w.caml_fresh_oo_id(0)];function
bm(e,g){var
f=c(k[82],e,g),h=f[2],a=c(k[3],e,f[1]);switch(a[0]){case
10:var
d=[1,a[1][1]];break;case
11:var
d=[2,a[1][1]];break;case
12:var
d=[3,a[1][1]];break;default:throw an}return[0,b(aX[41],d),h]}var
cG=[0,c(l[25],bl,cF),0],cH=c(l[25],[0,bl,0],cG),cJ=c(l[25],cI,cH),cK=c(l[25],D[6],cJ),cL=c(l[25],D[5],cK),cM=c(l[25],[0,cE,0],cL),cN=c(l[25],D[7],cM);function
F(a){var
c=p(D[4],cQ,D[7],a),d=b($[50],c);return b(k[8],d)}function
m(a){var
c=p(D[4],cR,cN,a),d=b($[50],c);return b(k[8],d)}function
ao(a){var
c=p(D[4],cS,cP,a),d=b($[50],c);return b(k[8],d)}function
G(a){var
c=p(D[4],cT,cO,a),d=b($[50],c);return b(k[8],d)}var
cV=[d,function(a){return F(cU)}],cX=[d,function(a){return F(cW)}],cZ=[d,function(a){return F(cY)}],c1=[d,function(a){return F(c0)}],c3=[d,function(a){return F(c2)}],c5=[d,function(a){return F(c4)}],c7=[d,function(a){return F(c6)}],c9=[d,function(a){return m(c8)}],c$=[d,function(a){return m(c_)}],db=[d,function(a){return m(da)}],dd=[d,function(a){return m(dc)}],df=[d,function(a){return m(de)}],dh=[d,function(a){return m(dg)}],dj=[d,function(a){return m(di)}],dl=[d,function(a){return m(dk)}],dn=[d,function(a){return m(dm)}],dq=[d,function(a){return m(dp)}],ds=[d,function(a){return m(dr)}],du=[d,function(a){return m(dt)}],dw=[d,function(a){return m(dv)}],dy=[d,function(a){return m(dx)}],dA=[d,function(a){return m(dz)}],dC=[d,function(a){return m(dB)}],dE=[d,function(a){return m(dD)}],dG=[d,function(a){return m(dF)}],dI=[d,function(a){return m(dH)}],dK=[d,function(a){return m(dJ)}],dM=[d,function(a){return m(dL)}],dO=[d,function(a){return m(dN)}],dQ=[d,function(a){return m(dP)}],dS=[d,function(a){return m(dR)}],dU=[d,function(a){return m(dT)}],dW=[d,function(a){return m(dV)}],dY=[d,function(a){return m(dX)}],d0=[d,function(a){return m(dZ)}],d2=[d,function(a){return m(d1)}],d4=[d,function(a){return m(d3)}],d6=[d,function(a){return m(d5)}],d8=[d,function(a){return m(d7)}],d_=[d,function(a){return m(d9)}],ea=[d,function(a){return m(d$)}],ap=[d,function(a){return F(eb)}],aq=[d,function(a){return F(ec)}];function
bn(a){if(0===a){var
c=q(aq);return r===c?aq[1]:d===c?b(o[2],aq):aq}var
e=q(ap),f=[0,bn(a-1|0)],g=r===e?ap[1]:d===e?b(o[2],ap):ap;return b(k[21],[0,g,f])}function
bo(c){var
a=p(D[2],ee,ed,c),d=b(ef[44],a)?function(a){var
c=b(bp[29][3],[0,a]);return b(k[2][1],c)}:function(a){return k[2][3]};return function(c){var
e=d(c),f=[0,b(eg[10],a),e];return b(k[28],f)}}function
bq(d,c,a){function
e(a){if(a){var
h=a[1],i=[0,h,e(a[2])],f=[0,b(bo(eh),d),[0,c]],j=[0,b(k[21],f),i];return b(k[21],j)}var
g=[0,b(bo(ei),d),[0,c]];return b(k[21],g)}return e(a)}var
ej=b($[14],0);function
ek(a){return bq(ej,k[14],a)}var
el=bp[1][1];function
em(a,b){return bq(el,a,b)}var
aa=[d,function(a){return G(et)}],ar=[d,function(a){return G(eu)}],as=[d,function(a){return G(ev)}],at=[d,function(a){return G(ew)}],au=[d,function(a){return G(ex)}],av=[d,function(a){return G(ey)}],aw=[d,function(a){return G(ez)}],ax=[d,function(a){return G(eA)}],ay=[d,function(a){return G(eB)}];function
az(a){if(c(g[17],a,g[6])){var
e=q(ar);return r===e?ar[1]:d===e?b(o[2],ar):ar}var
f=c(g[15],a,g[7]),l=f[2],m=[0,az(f[1])];if(c(g[17],l,g[5]))var
h=q(as),n=r===h?as[1]:d===h?b(o[2],as):as,i=n;else
var
j=q(at),p=r===j?at[1]:d===j?b(o[2],at):at,i=p;return b(k[21],[0,i,m])}function
eC(a){if(0===a){var
c=q(ax);return r===c?ax[1]:d===c?b(o[2],ax):ax}var
e=q(ay),f=[0,az(b(g[3],a))],h=r===e?ay[1]:d===e?b(o[2],ay):ay;return b(k[21],[0,h,f])}var
eE=[d,function(a){return ao(eD)}],eG=[d,function(a){return ao(eF)}],eI=[d,function(a){return ao(eH)}],eK=[d,function(a){return ao(eJ)}];function
br(i,a){function
d(j){var
f=bm(i,j),a=f[2],e=b(s[1][8],f[1]);if(h(e,eL)){if(h(e,eM)){if(!h(e,eN))if(a)if(!a[2]){var
k=d(a[1]);return c(g[14],g[7],k)}}else
if(a)if(!a[2]){var
l=d(a[1]),m=c(g[14],g[7],l);return c(g[12],g[6],m)}}else
if(!a)return g[6];throw an}try{var
e=[0,d(a)];return e}catch(a){a=u(a);if(a===an)return 0;throw a}}function
bs(f,k){try{var
j=bm(f,k),d=j[2],i=b(s[1][8],j[1]);if(h(i,eO))if(h(i,eP))if(h(i,eQ))var
a=0;else
if(d)if(d[2])var
a=0;else
var
e=br(f,d[1]),a=1;else
var
a=0;else
if(d)if(d[2])var
a=0;else
var
l=br(f,d[1]),e=c(O[16],g[22],l),a=1;else
var
a=0;else
if(d)var
a=0;else
var
e=[0,g[5]],a=1;if(!a)var
e=0;return e}catch(a){a=u(a);if(a===an)return 0;throw a}}function
bt(a){if(c(g[17],a,g[5])){var
e=q(au);return r===e?au[1]:d===e?b(o[2],au):au}if(b(g[18],a)){var
f=q(av),i=[0,az(a)],j=r===f?av[1]:d===f?b(o[2],av):av;return b(k[21],[0,j,i])}var
h=q(aw),l=[0,az(b(g[22],a))],m=r===h?aw[1]:d===h?b(o[2],aw):aw;return b(k[21],[0,m,l])}function
eR(p,o){var
a=am(p,o);if(typeof
a!=="number"&&1===a[0]){var
c=a[1];if(h(c,eS))if(h(c,eT))if(h(c,eU))if(h(c,eV))if(h(c,eW))if(h(c,eX)){var
s=h(c,eY)?h(c,eZ)?h(c,e0)?1:0:0:0;if(!s){var
q=bs(p,o);return q?[5,q[1]]:0}}else{var
d=a[2];if(d)if(!d[2])return[4,d[1]]}else{var
e=a[2];if(e){var
f=e[2];if(f)if(!f[2])return[2,e[1],f[1]]}}else{var
i=a[2];if(i)if(!i[2]){var
r=i[1];return[0,r,bt(b(g[22],g[6]))]}}else{var
j=a[2];if(j)if(!j[2])return[3,j[1]]}else{var
k=a[2];if(k){var
l=k[2];if(l)if(!l[2])return[1,k[1],l[1]]}}else{var
m=a[2];if(m){var
n=m[2];if(n)if(!n[2])return[0,m[1],n[1]]}}}return 0}function
bu(f,e){var
a=q(aa),g=r===a?aa[1]:d===a?b(o[2],aa):aa,h=e1[79];function
i(a){return c(h,0,a)}return Y(aA[42][1],i,f,e,g)}function
e2(B,A){var
C=b(ab[66][6],B),c=am(C,A);if(typeof
c!=="number"&&1===c[0]){var
e=c[1];if(h(e,e3))if(h(e,e4))if(h(e,e5))if(h(e,e6))if(h(e,e7)){if(!h(e,e8)){var
n=c[2];if(n){var
o=n[2];if(o){var
p=o[2];if(p)if(!p[2]){var
D=p[1],E=o[1];if(bu(B,n[1]))return[0,E,D]}}}}}else{var
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
a=am(C,A);if(typeof
a!=="number")switch(a[0]){case
1:var
d=a[1];if(h(d,en)){if(h(d,eo))if(h(d,ep))if(h(d,eq))if(h(d,er)){if(!h(d,es)){var
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
b=am(a,q);if(typeof
b!=="number"&&1===b[0]){var
d=b[1];if(h(d,e9))if(h(d,e_))if(h(d,e$))if(h(d,fa))if(h(d,fb))if(h(d,fc)){var
D=h(d,fd)?h(d,fe)?h(d,ff)?1:0:0:0;if(!D)return bs(a,q)}else{var
e=b[2];if(e)if(!e[2]){var
r=E(a,e[1]);return c(O[16],g[9],r)}}else{var
f=b[2];if(f){var
i=f[2];if(i)if(!i[2]){var
s=f[1],t=E(a,i[1]),u=E(a,s);return p(O[29],g[13],u,t)}}}else{var
j=b[2];if(j)if(!j[2]){var
v=E(a,j[1]);return c(O[16],g[10],v)}}else{var
k=b[2];if(k)if(!k[2]){var
w=E(a,k[1]);return c(O[16],g[22],w)}}else{var
l=b[2];if(l){var
m=l[2];if(m)if(!m[2]){var
x=l[1],y=E(a,m[1]),z=E(a,x);return p(O[29],g[14],z,y)}}}else{var
n=b[2];if(n){var
o=n[2];if(o)if(!o[2]){var
A=n[1],B=E(a,o[1]),C=E(a,A);return p(O[29],g[12],C,B)}}}}return 0}var
e=[0,cV,cX,cZ,c1,c3,c5,c7,c9,c$,db,dd,df,dh,dj,dl,dn,dq,ds,du,dw,dy,dA,dC,dE,dG,dI,dK,dM,dO,dQ,dS,dU,dW,dY,d0,d2,d4,d6,d8,d_,ea,bn,eC,em,ek,[0,aa,bu,eE,eG,eI,eK,bt,eR,e2,E]];aP(226,e,"Romega_plugin.Const_omega");var
v=b(fg[1][2],[0,g[17],g[16],g[12],g[13],g[14],g[15],g[22],g[5],g[6],g[2]]),fh=0,fi=0,fj=x[1],fk=[0,function(b,a){return b===a?1:0},fj],z=b(fl[19],fk),S=[0,0],bv=T[66][2];function
fm(a){b(l[32],a);b(l[35],0);return b(l[51],l[27])}var
U=T[66][3];function
bw(c,a){switch(c){case
0:var
b=0===a?1:0;break;case
1:var
b=1===a?1:0;break;default:var
b=2<=a?1:0}return b?1:0}function
bx(l,k){var
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
j=bx(e,h);if(j){var
b=f,a=i;continue}return j}return 0}}function
aB(c,a){if(0===c[0]){var
f=c[1];if(0===a[0])var
e=a[1],d=f,b=1;else
var
b=0}else{var
g=c[1];if(0===a[0])var
b=0;else
var
e=a[1],d=g,b=1}return b?d===e?1:0:0}var
ac=b(s[1][6],fn);function
by(d){var
a=b(z[1],7),c=b(z[1],7);return[0,0,0,b(z[1],7),0,c,a]}function
bz(a){a[4]=a[4]+1|0;return a[4]}function
bA(a){return 0===a[0]?[1,a[1]]:[0,a[1]]}function
fo(a){return a[1]}function
bB(d){function
a(f,e,d){if(d){var
h=d[2],j=d[1],g=b(fp[6],0),k=g[2],l=g[1],m=p(i[4],fq,f,e),n=a(f,e+1|0,h),o=b(t[5],0),q=p(fr[15],k,l,j),r=b(t[3],fs),s=b(t[3],m),u=b(t[13],0),v=c(t[12],u,s),w=c(t[12],v,r),x=c(t[12],w,q),y=c(t[12],x,o);return c(t[12],y,n)}return b(t[3],ft)}var
e=a(80,0,d[2]),f=b(t[5],0),g=b(t[3],fu),h=c(t[12],g,f),j=c(t[12],h,e),k=a(86,0,d[1]),l=b(t[5],0),m=b(t[3],fv),n=c(t[12],m,l),o=c(t[12],n,k),q=b(t[5],0),r=c(t[12],j,q),s=c(t[12],r,o);return c(fw[10],0,s)}var
aY=[0,-1];function
bC(a){aY[1]=-1;return 0}function
aZ(a){aY[1]++;return aY[1]}var
aC=[0,-1];function
bD(a){aC[1]=a;return 0}function
bE(a){aC[1]=-1;return 0}function
bF(a){aC[1]++;return aC[1]}function
aD(a){return c(i[4],fx,a)}function
a0(d,e,a){try{var
h=a[1],i=b(k[94],d),j=p(f[17][86],i,e,h);return j}catch(d){d=u(d);if(d===H){var
g=b(f[17][1],a[1]);a[1]=c(f[18],a[1],[0,e,0]);return g}throw d}}function
a1(a){try{var
c=b(f[17][7],a[1]);return c}catch(a){a=u(a);if(a[1]===bG)return b(l[2],fy);throw a}}function
bH(e,d,a){if(e===b(f[17][1],a[1])){a[1]=c(f[18],a[1],[0,d,0]);return 0}throw[0,ad,fz]}function
bI(d,a,e){try{var
h=a[2],i=b(k[94],d),j=p(f[17][86],i,e,h);return j}catch(d){d=u(d);if(d===H){var
g=b(f[17][1],a[2]);a[2]=c(f[18],a[2],[0,e,0]);return g}throw d}}function
fA(d,a){try{var
e=c(f[17][7],d,a);return e}catch(a){a=u(a);if(a[1]===bG)return b(l[2],fB);throw a}}function
bJ(b,a){var
d=a[7][1];return c(z[11],b[5],d)?0:p(z[5],b[5],d,a)}function
a2(a,b){try{var
d=c(z[7],a[5],b);return d}catch(a){a=u(a);if(a===H){c(i[2],fC,b);throw a}throw a}}function
C(c,a){switch(a[0]){case
0:var
d=b(g[2],a[1]);return p(i[1],c,fD,d);case
1:return Z(i[1],c,fE,C,a[1],C,a[2]);case
2:return Z(i[1],c,fF,C,a[1],C,a[2]);case
3:return Z(i[1],c,fG,C,a[1],C,a[2]);case
4:return Y(i[1],c,fH,C,a[1]);default:return p(i[1],c,fI,a[1])}}function
bK(a){switch(a){case
0:return fJ;case
1:return fK;case
2:return fL;case
3:return fM;case
4:return fN;default:return fO}}function
A(b,a){if(typeof
a==="number")return 0===a?c(i[1],b,fP):c(i[1],b,fQ);else
switch(a[0]){case
0:var
d=a[2],e=d[3],f=d[2],g=bK(d[1]);return b$(i[1],b,fR,C,f,g,C,e);case
1:return Y(i[1],b,fS,A,a[1]);case
2:return Z(i[1],b,fT,A,a[2],A,a[3]);case
3:return Z(i[1],b,fU,A,a[2],A,a[3]);case
4:return Z(i[1],b,fV,A,a[2],A,a[3]);default:return c(i[1],b,fW)}}function
bL(b){function
c(a){if(a){var
d=a[1],e=d[2],f=d[1];return[1,[2,[5,e],[0,f]],c(a[2])]}return[0,b[4]]}return c(b[3])}function
n(a,e){var
c=q(a),f=r===c?a[1]:d===c?b(o[2],a):a;return b(k[21],[0,f,e])}function
bM(d,a){function
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
bN(d,b){try{var
a=c(z[7],d[3],b);return a}catch(a){a=u(a);if(a===H){c(i[2],fX,b);var
e=d[3],f=function(b,a){return p(i[2],fY,b,a)};c(z[12],f,e);throw H}throw a}}function
bO(a){switch(a[0]){case
1:var
b=e[9];return function(a){return n(b,a)};case
2:var
c=e[10];return function(a){return n(c,a)};case
3:var
d=e[12];return function(a){return n(d,a)};default:throw[0,ad,fZ]}}function
aE(c,a){switch(a[0]){case
0:var
d=[0,b(e[46][7],a[1])];return n(e[8],d);case
4:var
i=[0,aE(c,a[1])];return n(e[11],i);case
5:var
j=bN(c,a[1]),k=[0,b(e[43],j)];return n(e[13],k);default:var
f=a[1],g=aE(c,a[2]),h=[0,aE(c,f),g];return b(bO(a),h)}}function
ae(a,b){try{var
c=aE(a,b);return c}catch(a){a=u(a);C(l[28],b);throw a}}function
bP(a){switch(a){case
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
bQ(a){if(typeof
a!=="number")switch(a[0]){case
2:var
b=e[24];return function(a){return n(b,a)};case
3:var
c=e[25];return function(a){return n(c,a)};case
4:var
d=e[26];return function(a){return n(d,a)}}throw[0,ad,f0]}function
af(f,c,a){if(typeof
a==="number"){if(0===a){var
g=e[21],j=q(g);return r===j?g[1]:d===j?b(o[2],g):g}var
h=e[22],k=q(h);return r===k?h[1]:d===k?b(o[2],h):h}else
switch(a[0]){case
0:var
i=a[2],l=i[2],m=i[1],p=ae(c,i[3]),s=[0,ae(c,l),p];return b(bP(m),s);case
1:var
t=[0,af(f,c,a[1])];return n(e[23],t);case
5:var
x=bI(f,c,a[1]),y=[0,b(e[42],x)];return n(e[27],y);default:var
u=a[2],v=af(f,c,a[3]),w=[0,af(f,c,u),v];return b(bQ(a),w)}}function
aF(c,a,b){try{var
d=af(c,a,b);return d}catch(a){a=u(a);A(l[28],b);throw a}}function
bR(b,a){var
c=a[1],d=ae(b,a[2]),f=[0,ae(b,c),d];return n(e[15],f)}var
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
aG(a){if(a){var
b=a[1],d=aG(a[2]),e=c(B,I(b[3]),d);return c(B,I(b[2]),e)}return x[2][1]}function
K(b){var
a=b;for(;;){if(typeof
a!=="number")switch(a[0]){case
0:return aG([0,a[2],0]);case
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
d=a[1];return[0,d,c(v[8],a[2],b)]}return[0,c(f[17][15],g,d),e]}function
bS(b,a){function
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
i=a[1],j=V(a[2]);return bS(V(i),j);case
2:var
b=a[1],e=a[2];if(0===e[0])var
d=e[1],f=b;else{if(0!==b[0])throw[0,ad,f1];var
d=b[1],f=a[2]}return c(g[17],d,g[5])?[0,0,v[11]]:a3(d,V(f));case
3:var
a=[1,a[1],[4,a[2]]];continue;case
4:var
k=V(a[1]);return a3(v[14],k);default:return[0,[0,[0,a[1],g[6]],0],g[5]]}}function
bT(i,b,a){var
d=a[2],e=a[1];function
g(a){return[0,a[2],a[1]]}var
h=c(f[17][15],g,e);return[0,aZ(0),b,h,d]}function
bU(a){switch(a){case
0:return 5;case
1:return 3;case
2:return 4;case
3:return 1;case
4:return 2;default:return 0}}function
bV(j,g,h,c,a){var
i=g[1],k=g[4],l=g[3],m=g[2];function
d(e,d){var
g=bT(j,d,V(e));return[0,h,c,a,[0,l,b(f[17][9],k)],i,m,g]}try{var
n=i?bU(h):h;switch(n){case
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
e=d([1,c,[4,a]],2)}return e}catch(a){a=u(a);if(b(f2[5],a))throw a;throw a}}function
bW(c,b,a){return[2,c,b,a]}function
a4(c,b,a){return[3,c,b,a]}function
bX(c,b,a){return[4,c,b,a]}function
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
ag(k,c,a,g,o,j,n,m,l){var
h=a[4],i=a[3],d=a[2],q=a[1],e=bz(c),r=g?[0,[0,e],d]:d,s=g?[0,[1,e],d]:d;if(g){var
t=[0,i,b(f[17][9],h)];p(z[5],c[6],e,t)}var
u=W(k,c,[0,o,r,i,[0,0,h]],j,m);return p(n,e,u,W(k,c,[0,q,s,i,[0,1,h]],j,l))}function
L(b,a,h,g,f,e,d){var
i=J(b,a,e),c=bV(a,h,f,i,J(b,a,d));bJ(a,c);return[0,g,c]}function
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
7:return ag(f,d,b,1-g,g,i,bW,a[1],a[2]);case
8:return ag(f,d,b,g,g,i,a4,a[1],a[2]);case
9:return ag(f,d,b,1-g,1-g,i,bX,a[1],a[2]);default:var
j=a[2],l=a[1],p=c(k[33],j,l);return ag(f,d,b,g,g,i,a4,c(k[33],l,j),p)}}function
bY(e,d,a){b(i[2],f3);p(i[2],f4,A,d);function
g(a){var
c=a[3],d=b(s[1][8],a[1]);return Y(i[2],f5,d,A,c)}c(f[17][14],g,a);return bB(e)}function
bZ(j,i,h,a){var
l=[0,0,0,a,0],f=c(aA[42][15],a,h);if(0===f[0])var
m=f[2];else{var
n=f[3],s=f[2];if(c(e[46][2],h,n)){var
g=e[5],p=q(g),t=r===p?g[1]:d===p?b(o[2],g):g;return[0,a,0,L(j,i,l,t,0,b(k[10],a),s)]}var
m=n}return[0,a,1,W(j,i,l,h,m)]}function
b0(d,a){var
e=b(ab[66][6],a),i=b(aA[42][6],a),j=b(aA[42][12],a),g=W(e,d,[0,1,0,ac,f6],a,i);function
k(b){return bZ(e,d,a,b)}var
h=c(f[17][15],k,j);if(S[1])bY(d,g,h);return[0,g,h]}function
be(e,b,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,[0,a[2],b],0];case
1:var
d=a[1];return e<50?bd(e+1|0,b,d):ca(bd,[0,b,d]);case
2:var
g=a[3],h=M(b,a[2]),i=M(b,g);return c(f[18],h,i);case
3:var
j=a[3],k=M(b,a[2]),l=function(a){return M(a,j)};return c(f[17][bk],l,k);case
4:var
m=a[3],n=P(b,a[2]),o=M(b,m);return c(f[18],n,o)}return[0,b,0]}function
bd(e,b,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,[0,a[2],b],0];case
1:var
d=a[1];return e<50?be(e+1|0,b,d):ca(be,[0,b,d]);case
2:var
g=a[3],h=P(b,a[2]),i=function(a){return P(a,g)};return c(f[17][bk],i,h);case
3:var
j=a[3],k=P(b,a[2]),l=P(b,j);return c(f[18],k,l);case
4:var
m=a[3],n=M(b,a[2]),o=function(a){return P(a,m)};return c(f[17][bk],o,n)}return[0,b,0]}function
M(a,b){return cb(be(0,a,b))}function
P(a,b){return cb(bd(0,a,b))}function
a6(a){if(a){var
b=a[2],c=M(0,a[1][3]),d=a6(b);return p(f[17][136],f[18],c,d)}return f7}function
aH(a){return 0===a[0]?c(i[2],f8,a[1]):c(i[2],f9,a[1])}function
b1(a){function
g(a){var
g=e[7],j=q(g),t=r===j?g[1]:d===j?b(o[2],g):g;A(l[27],[0,t,a]);b(l[35],0);var
h=a[7],k=b(v[31],h[2]),m=[0,h[3],h[4]];function
n(a){return b(v[29],aD)}hc(i[2],f_,h[1],n,m,k);b(i[2],f$);c(f[17][14],aH,a[6]);var
u=a[4][2];function
w(a){switch(a){case
0:return ga;case
1:return gb;default:return gc}}var
x=c(f[17][15],w,u),y=c(f[15][7],gd,x);c(i[2],ge,y);var
z=a[5]?gf:gh,B=b(s[1][8],a[4][1]);return p(i[2],gg,B,z)}function
h(a){b(i[2],gi);return c(f[17][14],g,a)}return c(f[17][14],h,a)}function
ah(e){var
a=e;for(;;){if(a){var
d=a[2],b=a[1];switch(b[0]){case
6:var
f=b[1],g=ah(d);return c(x[2][4],f[1],g);case
15:var
h=b[2][2],i=ah(b[3][2]);return c(B,ah(h),i);default:var
a=d;continue}}return x[2][1]}}function
b2(b,a){return cc(b[5],a[5])}var
X=b(f[20][1],[0,b2]);function
ai(d){var
a=d;for(;;){if(a){var
b=a[1];switch(b[0]){case
5:var
e=b[1],f=ai(a[2]);return c(X[4],e,f);case
15:if(!a[2]){var
g=b[2][2],h=ai(b[3][2]),i=ai(g);return c(X[7],i,h)}break}var
a=a[2];continue}return X[1]}}function
aI(a){if(0===a[0])return ai(a[1][3]);var
b=a[2],d=aI(a[3]),e=aI(b);return c(X[7],e,d)}function
a7(f){var
a=e[46][1],c=q(a),g=r===c?a[1]:d===c?b(o[2],a):a;return n(e[1],[0,g,f])}function
b3(c,d){function
e(a,b){var
f=b[4],g=b[3],h=b[2],i=b[1],d=bL(a[2]),e=bM(c,d);bH(a[5],e,c);var
j=a7(e);return[0,[0,a[5],i],[0,j,h],[0,[0,d,[5,a[5]]],g],[0,[1,a[2][1]],f]]}var
g=aI(d),a=p(X[15],e,g,gj),h=a[3],i=a[2],j=a[1],k=b(f[17][9],a[4]),l=b(f[17][9],h),m=b(f[17][9],i);return[0,b(f[17][9],j),m,l,k]}function
a8(c,a){if(a){var
e=a[2],g=a[1];try{var
j=a2(c,g)[6],d=j}catch(a){a=u(a);if(a!==H)throw a;var
d=0}var
h=a8(c,e),i=b(f[17][9],d);return p(f[17][59],aB,i,h)}return 0}function
b4(a){function
d(c,a){var
d=c[2],e=b(f[17][1],a[2]);return cc(b(f[17][1],d),e)}try{var
e=c(f[17][46],d,a),g=b(f[17][5],e);return g}catch(a){a=u(a);if(a[1]===gk)return b(l[2],gl);throw a}}function
b5(d,a){function
e(g){var
a=g;for(;;){if(a){var
c=a[2],b=a[1];if(p(f[17][55],aB,b,d)){var
a=c;continue}var
h=bA(b);if(p(f[17][55],aB,h,d))throw l[3];return[0,b,e(c)]}return 0}}function
b(a){var
b=a[2],c=a[1];try{var
d=[0,[0,c,e(b)]];return d}catch(a){a=u(a);if(a===l[3])return 0;throw a}}return c(f[17][70],b,a)}function
aJ(a){if(0===a[0])return a[1][2];var
b=a[2],d=aJ(a[3]);return c(B,aJ(b),d)}function
b6(m,a){function
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
aK(d,a){if(0===a[0]){var
e=a[1],g=b(x[2][21],e[2]),h=c(f[17][15],l[21],g),j=c(f[15][7],gm,h),k=p(i[4],gn,e[1],j);return c(l[54],d,k)}return b$(i[1],d,go,a[1],aK,a[2],aK,a[3])}function
aL(a,d){function
e(g,d,c){if(c){var
h=c[1];if(0===h[0]){var
i=h[1],k=c[2],l=aL(a,b(f[17][9],[0,[1,i],d]));return[1,i,e(g,[0,[0,i],d],k),l]}var
j=h[1],m=e(g,[0,[1,j],d],c[2]);return[1,j,aL(a,b(f[17][9],[0,[0,j],d])),m]}return[0,g]}var
g=b5(d,a);try{var
h=b4(g)}catch(e){e=u(e);var
j=b(f[17][1],a),k=b(f[17][1],g);p(i[2],gp,k,j);c(f[17][14],aH,d);throw e}var
l=h[2],m=h[1];return e(m,b(f[17][9],d),l)}function
a9(i,m){var
d=0,a=m,n=i[2],o=i[1];a:for(;;){if(a){var
j=a[1];if(0===j[0]){var
k=j[1],r=a[2],t=k[2];if(c(s[1][1],o,k[1])){var
e=[0,t,n];for(;;){var
f=e[1];if(f){var
g=e[2];if(g){var
p=g[2],q=f[2];if(bw(f[1],g[1])){var
e=[0,q,p];continue}}var
h=0}else
var
h=[0,e[2]];if(h)return[0,d,h[1]];var
d=d+1|0,a=r;continue a}}}var
d=d+1|0,a=a[2];continue}return b(l[2],gq)}}function
a_(h){function
i(h){switch(h){case
0:var
a=e[35],f=q(a),i=r===f?a[1]:d===f?b(o[2],a):a;return[0,i];case
1:var
c=e[36],g=q(c),j=r===g?c[1]:d===g?b(o[2],c):c;return[0,j];default:return 0}}var
j=c(f[17][70],i,h),a=e[34],g=q(a),k=r===g?a[1]:d===g?b(o[2],a):a;return c(e[44],k,j)}function
y(h,f){var
d=0,a=h;for(;;){if(a){var
g=a[1];if(1===g[0])if(f===g[1])return b(e[42],d);var
d=d+1|0,a=a[2];continue}var
j=c(i[4],gr,f);return b(l[2],j)}}function
a$(l,j,i){var
m=[0,b(e[42],0)],a=e[28],f=q(a),n=r===f?a[1]:d===f?b(o[2],a):a,p=b(k[21],[0,n,m]),s=l?v[14]:g[6],t=b(e[46][7],s),u=[0,b(e[46][7],g[6]),j,t,i,p],c=e[31],h=q(c),w=r===h?c[1]:d===h?b(o[2],c):c;return b(k[21],[0,w,u])}function
N(h,c,L){var
f=L;for(;;){if(f){var
a=f[1];switch(a[0]){case
0:var
z=f[2],x=a[3],w=a[1],i=1;break;case
1:if(!f[2]){var
R=a[1],S=b(e[46][7],a[2]),T=[0,y(c,R[1]),S],m=e[30],B=q(m),U=r===B?m[1]:d===B?b(o[2],m):m;return b(k[21],[0,U,T])}var
i=0;break;case
3:var
z=f[2],x=a[2],w=a[1],i=1;break;case
4:var
C=a[3],D=a[2],V=C[2],W=C[1],X=D[2],Y=D[1],Z=N(h,[0,[1,a[1]],c],f[2]),_=y(c,V[1]),$=b(e[46][7],W),aa=y(c,X[1]),ab=[0,b(e[46][7],Y),aa,$,_,Z],n=e[31],E=q(n),ac=r===E?n[1]:d===E?b(o[2],n):n;return b(k[21],[0,ac,ab]);case
5:var
p=a[1],ad=p[4],ae=p[3],af=p[2],ag=N(h,[0,[1,p[1][1]],c],f[2]),ah=y(c,af[1]),ai=b(e[46][7],ad),aj=y(c,ae[1]),ak=[0,b(e[46][7],g[6]),aj,ai,ah,ag],s=e[31],F=q(s),al=r===F?s[1]:d===F?b(o[2],s):s;return b(k[21],[0,al,ak]);case
6:var
f=f[2];continue;case
9:if(!f[2]){var
am=a[1],an=y(c,a[2][1]);return a$(0,y(c,am[1]),an)}var
i=0;break;case
10:if(!f[2]){var
ao=a[3],ap=a[1],aq=y(c,a[2][1]);return a$(ao,y(c,ap[1]),aq)}var
i=0;break;case
11:var
ar=a[3],as=a[2],at=N(h,[0,[1,a[1]],c],f[2]),au=y(c,ar),av=[0,y(c,as[1]),au,at],t=e[32],G=q(t),aw=r===G?t[1]:d===G?b(o[2],t):t;return b(k[21],[0,aw,av]);case
15:var
I=a[3],J=a[2],az=I[2],aA=I[1],aB=a[1],aC=N(h,[0,[1,J[1]],c],J[2]),aD=N(h,[0,[1,aA],c],az),aE=[0,y(c,aB[1]),aC,aD],v=e[33],K=q(v),aF=r===K?v[1]:d===K?b(o[2],v):v;return b(k[21],[0,aF,aE]);case
16:return b(l[2],gt);case
2:case
7:case
8:var
f=f[2];continue;default:if(!f[2]){var
ax=[0,y(c,a[1])],u=e[28],H=q(u),ay=r===H?u[1]:d===H?b(o[2],u):u;return b(k[21],[0,ay,ax])}var
i=0}if(i){var
M=N(h,c,z),O=b(e[46][7],x),P=[0,y(c,w[1]),O,M],j=e[29],A=q(j),Q=r===A?j[1]:d===A?b(o[2],j):j;return b(k[21],[0,Q,P])}}return b(l[2],gs)}}function
ba(h,f,d,a){if(a){var
j=a[1],m=a[2];try{var
v=c(z[7],f[5],j),g=v}catch(a){a=u(a);if(a!==H)throw a;var
o=c(i[4],gx,j),g=b(l[2],o)}var
k=a9(g[4],d),p=k[2],q=k[1],r=ba(h,f,[0,[1,g[7][1]],d],m),s=a_(p),t=[0,b(e[42],q),s,r];return n(e[38],t)}var
w=[0,N(f,d,h)];return n(e[39],w)}function
aM(h,g,d){if(0===d[0]){var
j=d[1],o=b(x[2][21],j[2]);return ba(j[3],h,g,o)}var
k=d[1],p=d[3],q=d[2];try{var
E=c(z[7],h[6],k),a=E}catch(d){d=u(d);if(d!==H)throw d;var
r=c(i[4],gu,k),a=b(l[2],r)}var
m=a9(a,g),s=m[2],t=m[1],v=c(f[18],a[2],gv),w=[0,[0,a[1],v]],y=c(f[18],a[2],gw),A=aM(h,[0,[0,[0,a[1],y]],g],p),B=aM(h,[0,w,g],q),C=a_(s),D=[0,b(e[42],t),C,B,A];return n(e[37],D)}function
b7(j,e,h){function
k(a){return a[7]}var
m=c(f[17][15],k,h),a=c(v[68],[0,aZ,bF,aD],m),d=ah(a),g=a8(j,b(x[2][21],d));if(S[1]){c(i[2],gy,e);c(v[36],aD,a);b(l[30],gz);var
n=function(a){return c(i[2],gA,a)};c(x[2][13],n,d);b(l[30],gB);c(f[17][14],aH,g);b(i[2],gC)}return[0,[0,e,d,a],g]}function
b8(R,w,a,y,P){var
A=y[1],T=y[2];if(S[1])b(i[2],gD);function
V(b,c){return b7(a,b,c)}var
g=aL(c(f[17][16],V,P),0);if(S[1]){aK(l[27],g);b(l[35],0)}var
C=aJ(g),W=K(A),X=[0,s[1][10][1],W];function
Y(e,b){var
f=b[2],g=b[1],d=a2(a,e),h=c(B,aG([0,d,0]),f);return[0,c(s[1][10][4],d[4][1],g),h]}var
D=p(x[2][15],Y,C,X),Z=D[2],_=c(s[1][10][6],ac,D[1]),E=b(s[1][10][21],_),h=b3(a,g),$=h[4],aa=h[3],ab=h[2],ae=h[1],af=b(x[2][21],Z),ag=c(f[18],af,ae);function
F(d,c){if(c){var
e=c[1],f=c[2],g=b(a1(a),e);p(z[5],a[3],e,d);return[0,g,F(d+1|0,f)]}return 0}var
ah=F(0,ag),j=e[46][1],G=q(j),ai=r===G?j[1]:d===G?b(o[2],j):j,aj=c(e[44],ai,ah);function
ak(b){return bR(a,b)}var
al=c(f[17][15],ak,aa),am=aF(w,a,A);function
an(d){try{var
e=c(s[1][11][22],d,T)}catch(a){a=u(a);if(a===H)throw[0,ad,gE];throw a}if(0===e[1]){var
f=e[2],g=a7(b(k[10],d));return[0,aF(w,a,f),g]}var
h=e[2],i=b(k[10],d);return[0,aF(w,a,b6(C,h)),i]}var
ao=c(f[17][15],an,E),I=b(f[17][44],ao),ap=I[2],aq=I[1],ar=b(e[45],a[2]),as=c(f[18],al,aq),m=e[14],J=q(m),at=r===J?m[1]:d===J?b(o[2],m):m,au=[0,am,ar,aj,c(e[44],at,as)],av=n(e[40],au);function
L(a){return[0,a,0]}function
aw(a){return[0,L(a)]}var
ax=c(f[17][15],aw,E),ay=[0,[0,L(ac)],$],az=aM(a,c(f[18],ay,ax),g),t=e[7],M=q(t),aA=r===M?t[1]:d===M?b(o[2],t):t,aB=b(Q[86],aA);if(R)var
v=e[5],N=q(v),aC=0,aD=r===N?v[1]:d===N?b(o[2],v):v,O=c(Q[5],aD,aC);else
var
O=Q[67];var
aE=n(e[41],[0,az]),aH=b(Q[86],aE),aI=c(Q[5],av,2),aN=c(f[18],ab,ap);return c(U,c(U,c(U,c(U,c(U,b(Q[146],aN),aI),aH),bv),O),aB)}var
bb=[0,v,fh,fi,z,S,bv,fm,U,bw,bx,aB,ac,by,bz,bA,fo,bB,aZ,bC,bF,bE,bD,aD,a0,a1,bH,bI,fA,bJ,a2,C,bK,A,bL,n,bM,bN,bO,ae,bP,bQ,af,aF,bR,B,I,aG,K,a3,bS,V,bT,bU,bV,bW,a4,bX,J,a5,ag,L,W,bY,bZ,b0,M,P,a6,aH,b1,ah,b2,X,ai,aI,a7,b3,a8,b4,b5,aJ,b6,aK,aL,a9,a_,y,a$,N,aM,ba,b7,b8,function(i){function
a(c){b(D[3],gF);bC(0);bE(0);try{var
a=by(0),d=b0(a,c),e=d[2],g=d[1];bD(b(f[17][1],a[1])-1|0);var
h=a6([0,[0,ac,1,[1,g]],e]),k=s[1][11][1],l=function(b,a){return p(s[1][11][4],a[1],[0,a[2],a[3]],b)},m=p(f[17][18],l,k,e);if(S[1])b1(h);var
n=b8(i,b(ab[66][6],c),a,[0,g,m],h);return n}catch(a){a=u(a);if(a===v[28]){var
j=b(t[3],gG);return p(b9[6],0,0,j)}throw a}}return b(ab[66][9],a)}];aP(243,bb,"Romega_plugin.Refl_omega");b(gH[10],aN);function
aj(a){var
d=c(bc[17],s[1][6],gI),e=b(s[5][4],d),f=b(s[6][4],a),g=c(s[13][2],[0,e],f),h=b(aO[4][12],g);return b(aO[13][22],h)}function
ak(d,a){var
e=c(f[17][104],gJ[33],a);function
g(a){if(h(a,gK)){if(h(a,gL)){if(h(a,gM)){if(h(a,gN)){var
d=c(l[16],gO,a),e=b(t[3],d);return p(b9[6],0,0,e)}return aj(gP)}return aj(gQ)}return aj(gR)}return aj(gS)}var
i=c(bc[17],g,e),j=b(bb[94],d),k=c(T[66][3],Q[28],j),m=b(T[66][20],i),n=b(ab[59],m),o=b(T[66][30],n);return c(T[66][3],o,k)}var
gT=0,gV=[0,[0,gU,function(a){return ak(1,0)}],gT],gX=[0,[0,gW,function(a){return ak(0,0)}],gV];Y(aO[10][8],aN,gY,0,gX);var
gZ=0,g2=[0,[0,g1,function(a){return ak(0,g0)}],gZ];function
g3(a,b){return ak(0,c(bc[17],s[1][8],a))}var
g5=b(s[1][7],g4),g8=[0,[0,[5,b(g7[16],g6[8])]],g5],ha=[0,[0,[0,g$,[0,g_,[1,c(g9[11],0,g8),0]]],g3],g2];Y(aO[10][8],aN,hb,0,ha);var
b_=[0,aN,aj,ak];aP(251,b_,"Romega_plugin.G_romega");aP(252,[0,e,bb,b_],"Romega_plugin");return}
