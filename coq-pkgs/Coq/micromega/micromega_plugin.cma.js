function(IA){"use strict";var
mi="QMicromega",mG="*",hM="param.csdp",mB="__varmap",mF="diagonalize: not PSD",mc="csdp: error ",m3="monoid",m_="__p",hH="_vendor+v8.10+32bit/coq/plugins/micromega/mfourier.ml",hV='"',mR="__p%i",mS=",",m9=" 1 ",mQ="1\n",m8="__ff",mh="=",mA="find_pivot",mP="%s*%a",m2="Require Import ZArith Lia. Open Scope Z_scope.\n",hG='"\n',mp="buggy certificate",mE="lia",al="ZMicromega",mO=" * ",J="micromega",fx='command "',mz=")^2",mb="compare_num",hX="real_nonlinear_prover",mg=" [*] ",mN="__wit",m1=143,mf="Zero",hW="Lia",hL="; csdp ",mo="ERROR: no compatible proof",aK="",b0="RingMicromega",mM='Unfortunately Coq isn\'t aware of the presence of any "csdp" executable in the path. \n\n',hT="real nonlinear prover",hS="0",hR="> /dev/null",my="%a * %a",ma="__arith",dO="Reals",m7=", False\n",hK=438,aS=248,l_="<=",l$="QArith",hQ="cd ",mD=") * (",cT=" + ",x="Coq",m0="psatz_R",me=148,mZ=".",mm="Csdp packages are provided by some OS distributions; binaries and source code can be downloaded from https://projects.coin-or.org/Csdp",mn=" : ",mY="Bad logical fragment",i=246,ml="-%a",mC="(%a) * (%a)",m6="x%i",mL=118,$="Tauto",hP="%a + %a",mK=119,mJ="csdp: Problem is infeasible",hU=" Cannot find witness",mX="Z",mx="the use of a specialized external tool called csdp. \n\n",ar=" ",m5="e",ad=103,fw=120,mW="EnvRing",l9="t",hJ=".dat-s",md="PsatzZ",mw="-",m4="Rdefinitions",mv="Timeout",mI="D",mU="linear prover",mV="Depth",mk="(%a)^2",mH=" Skipping what remains of this tactic: the complexity of the goal requires ",mt="psatz_Q",mu='" exited ',fy="_vendor+v8.10+32bit/coq/plugins/micromega/certificate.ml",fz="%s",hO=".out",mr="psatz_Z",ms="Rpow_def",mq="nat",l8="%i",hN="sos",aJ="\n",hI="pure_sos",mT=0.693147180559945286,aB="VarMap",mj=250,B=IA.jsoo_runtime,H=B.caml_check_bound,fv=B.caml_compare,X=B.caml_equal,aR=B.caml_fresh_oo_id,a$=B.caml_int_compare,l5=B.caml_int_of_string,bZ=B.caml_lessthan,Ix=B.caml_list_of_js_array,cn=B.caml_ml_string_length,ft=B.caml_mul,c=B.caml_new_string,l6=B.caml_obj_tag,aA=B.caml_register_global,l4=B.caml_string_equal,bL=B.caml_string_get,cS=B.caml_sys_remove,hF=B.caml_sys_system_command,o=B.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):B.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):B.caml_call_gen(a,[b,c])}function
g(a,b,c,d){return a.length==3?a(b,c,d):B.caml_call_gen(a,[b,c,d])}function
C(a,b,c,d,e){return a.length==4?a(b,c,d,e):B.caml_call_gen(a,[b,c,d,e])}function
T(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):B.caml_call_gen(a,[b,c,d,e,f])}function
W(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):B.caml_call_gen(a,[b,c,d,e,f,g])}function
fu(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):B.caml_call_gen(a,[b,c,d,e,f,g,h])}function
l7(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):B.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
Iz(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):B.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
Iy(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):B.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}var
p=B.caml_get_global_data(),b6=[0,0,0],gb=[0,0],kg=Ix([[0,c(x),[0,c("Lists"),[0,c("List"),0]]],[0,c(x),[0,c(J),[0,c(al),0]]],[0,c(x),[0,c(J),[0,c($),0]]],[0,c(x),[0,c(J),[0,c("DeclConstant"),0]]],[0,c(x),[0,c(J),[0,c(b0),0]]],[0,c(x),[0,c(J),[0,c(mW),0]]],[0,c(x),[0,c(J),[0,c(al),0]]],[0,c(x),[0,c(J),[0,c("RMicromega"),0]]],[0,c(x),[0,c(J),[0,c($),0]]],[0,c(x),[0,c(J),[0,c(b0),0]]],[0,c(x),[0,c(J),[0,c(mW),0]]],[0,c(x),[0,c(l$),[0,c("QArith_base"),0]]],[0,c(x),[0,c(dO),[0,c(m4),0]]],[0,c(x),[0,c(dO),[0,c(ms),0]]],[0,c("LRing_normalise"),0]]),ax=c("micromega_plugin"),hh=[0,0],bk=[0,1],lv=c(" \t\n\r"),lw=c(",;"),lx=c("()[]{}"),ly=c("\\!@#$%^&*-+|\\<=>/?~.:"),lz=c("'abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ"),lA=c("0123456789"),lT=[0,1],hy=c("axtol=1.0e-8\natytol=1.0e-8\nobjtol=1.0e-8\npinftol=1.0e8\ndinftol=1.0e8\nmaxiter=100\nminstepfrac=0.9\nmaxstepfrac=0.97\nminstepp=1.0e-8\nminstepd=1.0e-8\nusexzgap=1\ntweakgap=0\naffine=0\nprintlevel=1\n"),d=p.Num,k=p.Stdlib__printf,e=p.Stdlib,O=p.Unix,ea=p.Stdlib__marshal,d7=p.Stdlib__printexc,f=p.Stdlib__list,l=p.Big_int,aV=p.Assert_failure,dg=p.Ratio,f8=p.Stdlib__set,b7=p.Stdlib__map,bc=p.Stdlib__hashtbl,L=p.Not_found,m=p.Util,cI=p.CErrors,aY=p.Option,jN=p.Invalid_argument,bD=p.Stdlib__filename,gN=p.CList,kb=p.CamlinternalLazy,ka=p.End_of_file,bU=p.Names,j=p.EConstr,aZ=p.Tacmach,N=p.Tacticals,bi=p.Pp,cM=p.Proofview,k7=p.CAst,aF=p.Tactics,e9=p.Failure,kH=p.Stdlib__array,kX=p.Context,kG=p.Retyping,kP=p.Evd,dw=p.Coqlib,dv=p.Goptions,ay=p.Ltac_plugin__Tacinterp,aG=p.Ltac_plugin__Tacentries,fa=p.Stdarg,aa=p.Genarg,az=p.Ltac_plugin__Tacarg,a9=p.Stdlib__string,tN=p.CMap,Ce=p.Coq_config,Ci=p.Envars,A7=p.Sorts,AQ=p.Redexpr,AB=p.UnivProblem,AC=p.Univ,Av=p.Reductionops,Af=p.Typeclasses,wJ=p.UnivGen,Cc=p.System,C2=p.Mltop,EN=p.Sys_error;aA(976,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"Micromega_plugin");var
ne=[0,1],m$=[0,[12,91,[2,0,0]],c("[%s")],nd=c("]-oo"),na=c(mS),nb=[0,[2,0,[12,93,0]],c("%s]")],nc=c("+oo["),oL=[0,0],oY=[0,0,0],oZ=[0,[0,0],0],o4=[0,[0,0],0],o5=[0,0,0],o2=[0,[0,0],0],o3=[0,0,0],oR=[0,[0,0],0],oS=[0,0,0],oP=[0,[0,0],0],oQ=[0,0,0],oJ=[0,1],oK=[0,0],oI=[0,0],oG=[0,0,1,0],oA=[0,0],oz=[0,0],oy=[0,0],oq=[0,[0,0]],or=[0,[0,0]],os=[0,[0,0]],ot=[0,[0,0]],om=[0,[0,0]],on=[0,[0,0]],oo=[0,[0,0]],op=[0,[0,0]],n5=[0,[0,0],0],n4=[0,0,0],nY=[0,1],nZ=[0,2],n0=[0,3],nT=[0,0],nV=[0,0],nU=[0,1],nX=[0,3],nW=[0,0],nS=[0,0,0],ny=[0,[1,0]],nz=[0,0,[0,0]],nA=[0,[0,0],0],nB=[0,0],nC=[0,[1,0]],nD=[0,[1,0]],nE=[0,0],nF=[0,[1,0]],nG=[0,[1,0]],nH=[0,[1,0]],nI=[0,0],nJ=[0,[1,0]],nK=[0,0,0],nL=[0,0,0],nM=[0,0],nN=[0,0,0],nO=[0,0],ns=[0,0],nq=[0,0],np=[1,0],no=[0,0],nh=[1,0],ni=[1,0],nj=[0,0],nl=[0,0],nk=[0,0],n_=[0,0],ok=[0,0],oE=[0,0],oN=[0,[0,0],0],oO=[0,0,0],oT=[0,0,0],oU=[0,0,0],oV=[0,[0,0],0],oW=[0,0,0],o0=[0,[0,0],0],o1=[0,0,0],o6=[0,0,0],o7=[0,0,0],qF=[0,[11,c(fx),[2,0,[11,c(mu),[4,3,0,0,0]]]],c('command "%s" exited %i')],qE=[0,[11,c(fx),[2,0,[11,c(mu),[2,0,0]]]],c('command "%s" exited %s')],qG=[0,[11,c(fx),[2,0,[11,c('" killed '),[4,3,0,0,0]]]],c('command "%s" killed %i')],qH=[0,[11,c(fx),[2,0,[11,c('" stopped '),[4,3,0,0,0]]]],c('command "%s" stopped %i')],qs=[0,c("_vendor+v8.10+32bit/coq/plugins/micromega/mutils.ml"),262,7],qo=[0,0,0],qn=[0,0,0],ql=[0,0,0],pJ=[0,[4,3,0,0,[12,32,0]],c("%i ")],qQ=[0,[15,[11,c(cT),[15,0]]],c(hP)],qR=c(hS),qY=c(mb),qZ=c(mb),q2=[0,0],q3=[0,0],q$=[0,0],ra=[0,0],ri=[0,1],rf=[0,0],re=[0,c("_vendor+v8.10+32bit/coq/plugins/micromega/vect.ml"),285,16],rb=[0,[0,0,[0,0]],0],q9=[0,0],q8=[0,0],q7=[0,0],q4=[0,1],q5=[0,1],q1=[0,1],q0=[0,0],qX=[0,0],qW=[0,0],qU=[0,[15,[12,32,0]],c("%a ")],qV=[0,[11,c("(+ "),[15,[12,41,0]]],c("(+ %a)")],qS=[0,[12,fw,[4,3,0,0,0]],c(m6)],qM=[0,0],qN=[0,[2,0,0],c(fz)],qP=[0,[11,c("(- "),[15,[12,41,0]]],c("(- %a)")],qO=[0,[11,c("(* "),[2,0,[12,32,[15,[12,41,0]]]]],c("(* %s %a)")],qI=[0,0],qJ=[0,[2,0,0],c(fz)],qL=[0,[12,45,[15,0]],c(ml)],qK=[0,[2,0,[12,42,[15,0]]],c(mP)],tn=c("pivot: equality as second argument"),to=[0,1],tp=[0,-1],tm=[0,1,0,0],ti=[0,0],tj=[0,-1],tk=c("cutting_plane ignore strict constraints"),tg=[0,0],tb=[0,[15,[12,10,0]],c("%a\n")],s$=[0,[15,[12,32,[2,0,[11,c(" 0 by "),[15,[12,10,0]]]]]],c("%a %s 0 by %a\n")],sy=[0,[11,c(mf),0],c(mf)],sz=[0,[12,40,[15,[12,41,[12,64,[2,0,0]]]]],c("(%a)@%s")],sA=[0,[11,c("Hyp "),[4,3,0,0,0]],c("Hyp %i")],sB=[0,[11,c("Def "),[4,3,0,0,0]],c("Def %i")],sC=[0,[11,c("Cst "),[2,0,0]],c("Cst %s")],sD=[0,[12,40,[15,[11,c(mz),0]]],c(mk)],sE=[0,[12,40,[15,[11,c(mD),[15,[12,41,0]]]]],c(mC)],sF=[0,[12,40,[15,[11,c(")/"),[2,0,0]]]],c("(%a)/%s")],sG=[0,[12,40,[15,[11,c(mD),[15,[12,41,0]]]]],c(mC)],sH=[0,[15,[11,c(cT),[15,0]]],c(hP)],sI=[0,[12,91,[15,[12,93,0]]],c("[%a]")],sJ=[0,[12,46,0],c(mZ)],sK=[0,[4,3,0,0,[11,c(":= "),[15,[11,c(" ; "),[15,0]]]]],c("%i:= %a ; %a")],sL=c(";"),sM=[0,[4,3,0,0,[12,123,[15,[11,c(l_),[15,[11,c(l_),[15,[12,125,[15,0]]]]]]]]],c("%i{%a<=%a<=%a}%a")],sO=[0,1],sP=[0,1],sN=[0,0],sQ=[0,c("_vendor+v8.10+32bit/coq/plugins/micromega/polynomial.ml"),695,14],sR=[0,1],sU=[0,1],sT=[0,1],sS=[0,1],s1=[0,0],s2=c("eval_prf_rule : negative constant"),s3=[0,[11,c("MulC("),[15,[12,44,[15,[11,c(") invalid 2d arg "),[15,[12,32,[2,0,0]]]]]]]],c("MulC(%a,%a) invalid 2d arg %a %s")],s4=c("eval_prf_rule : not an equality"),s7=c("Proof is not finished"),s8=[0,[11,c("Last inference "),[15,[12,32,[2,0,[12,10,0]]]]],c("Last inference %a %s\n")],s9=c("Not implemented"),s5=[0,0],sZ=c("Cuts should already be compiled"),sW=[0,[4,3,0,0,[12,44,[2,0,0]]],c("%i,%s")],sX=c(aK),sY=[0,[11,c("id_of_hyp "),[4,3,0,0,[12,32,[2,0,0]]]],c("id_of_hyp %i %s")],sv=[0,[11,c("(H"),[4,3,0,0,[11,c(mn),[15,[12,32,[2,0,[11,c(" 0)\n"),0]]]]]]],c("(H%i : %a %s 0)\n")],st=[0,[11,c("(x"),[4,3,0,0,[11,c(mn),[2,0,[11,c(") "),0]]]]],c("(x%i : %s) ")],su=[0,[11,c("forall "),[15,[12,10,0]]],c("forall %a\n")],sw=[0,[11,c(m7),0],c(m7)],sq=[0,0],sn=[0,[12,mL,[4,3,0,0,0]],c("v%i")],sl=[0,1],sm=[0,0],sk=[0,0],sj=[0,1],sh=[0,1],sf=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],c("%a %s %s")],sb=c(mh),sc=c(">="),sd=c(">"),r_=[0,0],r$=[0,0],r8=[0,0],r7=[0,1],r5=[0,0],r4=[0,[15,[11,c(cT),0]],c("%a + ")],r0=[0,0],r1=[0,[2,0,0],c(fz)],r3=[0,[12,45,[15,0]],c(ml)],r2=[0,[2,0,[12,42,[15,0]]],c(mP)],ro=[0,[15,[12,42,[15,0]]],c("%a*%a")],rm=[0,[12,fw,[4,3,0,0,0]],c(m6)],rn=[0,[12,fw,[4,3,0,0,[12,94,[4,3,0,0,0]]]],c("x%i^%i")],sa=c("Micromega_plugin.Polynomial.Strict"),tc=c("Micromega_plugin.Polynomial.WithProof.InvalidProof"),tt=[0,[12,72,[4,3,0,0,0]],c("H%i")],tu=[0,[11,c("E("),[4,3,0,0,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("E(%i,%a,%a)")],tv=[0,[11,c("A("),[15,[12,44,[15,[12,41,0]]]]],c("A(%a,%a)")],uh=[0,1],ui=[0,[0,0,0]],ue=c("merge_proof : pivot is not possible"),uf=[0,1],ug=[0,1],ud=[0,0],t9=[0,1],t_=[0,1],t$=[0,1],ua=[0,1],ub=[0,1],uc=[0,1],t5=[0,0],t6=[0,-1],t7=[0,[11,c("optimise Exception : "),[2,0,0]],c("optimise Exception : %s")],tV=[0,0],tW=[0,0],tX=[0,0],tY=[0,0],tZ=[0,0],t0=[0,0],t1=[0,0],t2=[0,0],tS=[0,[11,c("bound_of_variable: eval_vecr "),[15,[11,c(" = "),[2,0,[12,44,[15,[12,10,0]]]]]]],c("bound_of_variable: eval_vecr %a = %s,%a\n")],tT=[0,[11,c("current interval:  "),[15,[12,10,0]]],c("current interval:  %a\n")],tU=c("bound_of_variable: impossible"),tR=[0,0,0],tP=[0,0,0],tQ=c("SystemContradiction"),tO=[0,0],tL=[0,0],tK=[0,0,0,0],tE=[0,0],tF=[0,0],tI=[0,c(hH),200,4],tG=[0,1],tH=[0,1],tC=[0,c(hH),151,6],tB=[0,0,0],tA=[0,0],tx=[0,c(hH),95,4],ts=c("Micromega_plugin.Mfourier.SystemContradiction"),tz=c("Micromega_plugin.Mfourier.TimeOut"),un=[0,0],uo=c("find_pivot_column"),uZ=[0,1],u0=[0,0],uW=[0,1],uX=[0,1],uY=[0,1],uR=[0,0],uS=[0,2],uT=[0,1],uU=[0,1],uV=[0,1],u1=[0,1],uQ=[0,0],uP=[0,0],uO=[0,1],uL=[0,1],uK=[0,-1],uF=[0,0],uG=c("push_real"),uH=[0,-1],uI=[0,0],uJ=[0,1],uE=[0,0],uD=c(mA),uA=[0,0],uz=c("pivot"),uw=[0,0],ux=[0,0],uy=[0,1],ur=[0,0],us=c("Cannot solve column"),ut=[0,-1],uu=[0,0],uv=[0,-1],uq=c(mA),up=[0,0],ul=[0,0],uj=[0,[11,c("Cannot restrict "),[4,3,0,0,0]],c("Cannot restrict %i")],uM=c("Micromega_plugin.Simplex.FoundVar"),u2=c(hS),u3=[0,[12,mL,[2,0,0]],c("v%s")],u4=[0,[11,c("- ("),[15,[12,41,0]]],c("- (%a)")],u5=[0,[12,40,[15,[11,c(")+("),[15,[12,41,0]]]]],c("(%a)+(%a)")],u6=[0,[12,40,[15,[11,c(")-("),[15,[12,41,0]]]]],c("(%a)-(%a)")],u7=[0,[12,40,[15,[11,c(")*("),[15,[12,41,0]]]]],c("(%a)*(%a)")],u8=[0,[12,40,[15,[11,c(")^("),[4,3,0,0,[12,41,0]]]]],c("(%a)^(%i)")],u9=[0,[11,c("Aeq("),[4,3,0,0,[12,41,0]]],c("Aeq(%i)")],u_=[0,[11,c("Ale("),[4,3,0,0,[12,41,0]]],c("Ale(%i)")],u$=[0,[11,c("Alt("),[4,3,0,0,[12,41,0]]],c("Alt(%i)")],va=[0,[11,c("eq("),[2,0,[12,41,0]]],c("eq(%s)")],vb=[0,[11,c("le("),[2,0,[12,41,0]]],c("le(%s)")],vc=[0,[11,c("lt("),[2,0,[12,41,0]]],c("lt(%s)")],vd=[0,[12,40,[15,[11,c(mz),0]]],c(mk)],ve=[0,[11,c(m3),0],c(m3)],vf=[0,[15,[11,c(mO),[15,0]]],c(my)],vg=[0,[15,[11,c(cT),[15,0]]],c(hP)],vh=[0,[15,[11,c(mO),[15,0]]],c(my)],vI=c("scale term: not implemented"),vJ=[0,0],vN=[0,0],wc=c("nia"),wb=c(mE),v5=c(".v"),v6=[0,[11,c(m2),0],c(m2)],v7=c(mX),v8=[0,[11,c("Goal "),[15,[11,c(".\n"),0]]],c("Goal %a.\n")],v_=[0,[11,c("Proof.\n intros. "),[2,0,[11,c(".\nQed.\n"),0]]],c("Proof.\n intros. %s.\nQed.\n")],v9=[0,[11,c("Proof.\n intros. Fail "),[2,0,[11,c(".\nAbort.\n"),0]]],c("Proof.\n intros. Fail %s.\nAbort.\n")],v0=[0,c(fy),922,4],v1=[0,0],v2=[0,1],v3=[0,c(fy),951,4],vW=[0,1],vX=[0,0,0],vY=c("Interval without proof"),vT=[0,1],vU=[0,-1],vO=[0,0],vP=[0,1],vM=[0,0],vK=[0,1],vL=[0,0],vH=c("P"),vG=[0,1],vD=[0,1],vE=[0,-1],vz=[0,1],vA=[0,c(fy),339,11],vB=c("check_sat : Unexpected operator"),vC=[0,0],vx=[0,c(fy),302,14],vu=[0,0],vv=c("dual_raw_certificate: empty_certificate"),vr=[0,1],vq=[0,0],vk=[0,0],vn=[0,[0,0],0],vo=[0,0,0],vy=c("Micromega_plugin.Certificate.FoundProof"),wj=[0,0,0],wh=[0,0,0],wf=[0,0,[0,5,0]],wi=[0,1,[0,4,[0,5,0]]],wg=[0,1,[0,6,[0,5,0]]],wd=c("Micromega_plugin.Persistent_cache.PHashtable(Key).InvalidTableFormat"),we=c("Micromega_plugin.Persistent_cache.PHashtable(Key).UnboundTable"),Bs=[0,[12,68,0],c(mI)],Bt=[0,[11,c("R["),[15,[12,44,[15,[12,93,0]]]]],c("R[%a,%a]")],Bu=[0,[11,c("C["),[15,[12,44,[15,[12,93,0]]]]],c("C[%a,%a]")],Bv=c("]"),Bw=c("["),Bx=[0,[11,c("EP["),[15,[12,44,[15,[12,44,[15,[12,93,0]]]]]]],c("EP[%a,%a,%a]")],BI=c("abstract_wrt_formula"),C1=c("Not ground"),CR=c(hX),CQ=c(hX),CP=c(hX),CJ=c(hT),CI=c(hT),CH=c(hT),Cm=c(mp),Cl=c(mp),Cf=c("csdpcert"),Cg=c(J),Ch=c("plugins"),B9=c(ma),B_=[0,0],B8=c(hU),B2=c(mY),B3=c(mv),B4=c(mm),B5=c(mM),B6=c(mx),B7=c(mH),BW=c(mN),BX=c(l9),BY=[0,[0,c(x),[0,c(J),[0,c(aB),0]]],[0,[0,c(aB),0],0]],BZ=c(aB),B0=c(mB),B1=c(m8),BR=c(hU),BS=c(ma),BT=[0,0],BU=c(hU),BL=c(mY),BM=c(mv),BN=c(mm),BO=c(mM),BP=c(mx),BQ=c(mH),BG=[0,[11,c(mo),0],c(mo)],BH=c("Cannot find compatible proof"),BE=c("bad old index"),BF=c("proof compaction error"),BD=[0,0],BA=c(mN),BB=c(mB),BC=c(m8),Br=[0,[15,[12,47,[15,0]]],c("%a/%a")],Bn=c(l9),Bo=[0,[0,c(x),[0,c(J),[0,c(aB),0]]],[0,[0,c(aB),0],0]],Bp=c(aB),Bj=c("Empty"),Bk=[0,[0,c(x),[0,c(J),[0,c(aB),0]]],[0,[0,c(aB),0],0]],Bl=c(aB),Bf=c("Elt"),Bg=[0,[0,c(x),[0,c(J),[0,c(aB),0]]],[0,[0,c(aB),0],0]],Bh=c(aB),Bb=c("Branch"),Bc=[0,[0,c(x),[0,c(J),[0,c(aB),0]]],[0,[0,c(aB),0],0]],Bd=c(aB),AS=[0,0],Ba=[0,[11,c(m_),[4,3,0,0,0]],c(mR)],A$=[0,[11,c(m_),[4,3,0,0,0]],c(mR)],A_=[0,[11,c("__x"),[4,3,0,0,0]],c("__x%i")],A9=[0,c("_vendor+v8.10+32bit/coq/plugins/micromega/coq_micromega.ml"),1225,11],A1=c("error : parse_arith(2)"),AX=[0,0,0],AD=c("get_rank"),Az=[1,c("Oups")],Ao=[0,[12,48,0],c(hS)],Ap=[0,[11,c("(In "),[15,[12,41,[12,37,[11,c(mq),0]]]]],c("(In %a)%%nat")],Aq=[0,[12,40,[15,[11,c("^2)"),0]]],c("(%a^2)")],Ar=[0,[11,c("( "),[15,[11,c(mg),[15,[12,41,0]]]]],c("( %a [*] %a)")],As=[0,[12,40,[15,[11,c(mg),[15,[12,41,0]]]]],c("(%a [*] %a)")],At=[0,[12,40,[15,[11,c(" [+] "),[15,[12,41,0]]]]],c("(%a [+] %a)")],Au=[0,[12,40,[15,[12,41,[12,37,[11,c("positive"),0]]]]],c("(%a)%%positive")],Al=[0,[11,c("Pc "),[15,0]],c("Pc %a")],Am=[0,[11,c("Pinj("),[15,[12,44,[15,[12,41,0]]]]],c("Pinj(%a,%a)")],An=[0,[11,c("PX("),[15,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("PX(%a,%a,%a)")],Ai=[0,[15,[11,c(" ,"),[15,0]]],c("%a ,%a")],Aj=[0,[15,0],c("%a")],Ak=[0,[2,0,[15,[2,0,0]]],c("%s%a%s")],Ag=[0,[2,0,0],c(fz)],Ae=[0,[4,3,0,0,0],c(l8)],Ad=[0,[4,3,0,0,0],c(l8)],z_=c("Formula"),z$=[0,[0,c(x),[0,c(J),[0,c(b0),0]]],[0,[0,c(b0),0],0]],Aa=c(b0),z6=c("Build_Formula"),z7=[0,[0,c(x),[0,c(J),[0,c(b0),0]]],[0,[0,c(b0),0],0]],z8=c(b0),z3=c("QWitness"),z4=[0,[0,c(x),[0,c(J),[0,c(mi),0]]],0],z5=c(mi),z0=c("BFormula"),z1=[0,[0,c(x),[0,c(J),[0,c($),0]]],[0,[0,c($),0],0]],z2=c(al),zW=c("I"),zX=[0,[0,c(x),[0,c(J),[0,c($),0]]],[0,[0,c($),0],0]],zY=c(al),zS=c("X"),zT=[0,[0,c(x),[0,c(J),[0,c($),0]]],[0,[0,c($),0],0]],zU=c(al),zO=c("A"),zP=[0,[0,c(x),[0,c(J),[0,c($),0]]],[0,[0,c($),0],0]],zQ=c(al),zK=c("N"),zL=[0,[0,c(x),[0,c(J),[0,c($),0]]],[0,[0,c($),0],0]],zM=c(al),zG=c(mI),zH=[0,[0,c(x),[0,c(J),[0,c($),0]]],[0,[0,c($),0],0]],zI=c(al),zC=c("Cj"),zD=[0,[0,c(x),[0,c(J),[0,c($),0]]],[0,[0,c($),0],0]],zE=c(al),zy=c("FF"),zz=[0,[0,c(x),[0,c(J),[0,c($),0]]],[0,[0,c($),0],0]],zA=c(al),zu=c("TT"),zv=[0,[0,c(x),[0,c(J),[0,c($),0]]],[0,[0,c($),0],0]],zw=c(al),zs=c("DeclaredConstant"),zq=c(md),zo=c("PsatzC"),zm=c("PsatzAdd"),zk=c("PsatzMulC"),zi=c("PsatzMulE"),zg=c("PsatzSquare"),ze=c("PsatzIn"),zc=c("OpGt"),za=c("OpGe"),y_=c("OpLt"),y8=c("OpLe"),y6=c("OpNEq"),y4=c("OpEq"),y2=c("Pinj"),y0=c("Pc"),yY=c("PX"),yW=c("PEpow"),yU=c("PEsub"),yS=c("PEmul"),yQ=c("PEopp"),yO=c("PEadd"),yM=c("PEc"),yK=c("PEX"),yJ=c("Q2R"),yI=c("IZR"),yG=c("powerRZ"),yF=c("pow"),yE=c("Rinv"),yD=c("Rmult"),yC=c("Ropp"),yB=c("Rminus"),yA=c("Rplus"),yy=c("Rlt"),yw=c("Rle"),yu=c("Rge"),ys=c("Rgt"),yr=c("Qpower"),yq=c("Qmult"),yp=c("Qopp"),yo=c("Qminus"),yn=c("Qplus"),yl=c("Qeq"),yj=c("Qlt"),yh=c("Qle"),yg=c("Z.pow"),yf=c("Z.mul"),ye=c("Z.opp"),yd=c("Z.sub"),yc=c("Z.add"),yb=c("eq"),x$=c("Z.lt"),x9=c("Z.le"),x7=c("Z.ge"),x5=c("Z.gt"),x3=c("EnumProof"),x1=c("CutProof"),xZ=c("RatProof"),xX=c("DoneProof"),xW=c("ZArithProof"),xV=c("R1"),xU=c("R0"),xS=c("COpp"),xQ=c("CInv"),xO=c("CPow"),xM=c("CMult"),xK=c("CMinus"),xI=c("CPlus"),xG=c("CZ"),xE=c("CQ"),xC=c("C1"),xA=c("C0"),xz=c("Rcst"),xy=c("Qmake"),xx=c("R"),xw=c("Q"),xu=c("Zneg"),xs=c("Zpos"),xq=c("Z0"),xp=c(mX),xn=c("xI"),xl=c("xO"),xj=c("xH"),xh=c("Npos"),xf=c("N0"),xd=c("inr"),xb=c("inl"),w$=c("tt"),w9=c("None"),w8=c("unit"),w7=c(mq),w5=c("S"),w3=c("O"),w2=c("list"),w0=c("nil"),wY=c("cons"),wX=c("False"),wW=c("True"),wU=c("iff"),wT=c("not"),wS=c("or"),wR=c("and"),wk=c(aK),wm=[0,c(hW),[0,c("Enum"),0]],wn=c("Lia Enum"),wq=[0,c("Simplex"),0],wr=c("Use the Simplex instead of Fourier elimination"),wu=[0,c("Dump"),[0,c("Arith"),0]],wv=c("Generate Coq goals in file from calls to 'lia' 'nia'"),wx=[0,c("Lra"),[0,c(mV),0]],wz=[0,c(hW),[0,c(mV),0]],wB=[0,c(x),[0,c("Logic"),[0,c("Decidable"),0]]],wG=[0,[0,c(x),[0,c("Numbers"),[0,c("BinNums"),0]]],0],wH=[0,[0,c(x),[0,c(dO),[0,c(m4),0]]],[0,[0,c(x),[0,c(dO),[0,c(ms),0]]],[0,[0,c(x),[0,c(dO),[0,c("Raxioms"),0]]],[0,[0,c(x),[0,c(l$),[0,c("Qreals"),0]]],0]]]],wI=[0,[0,c(x),[0,c("ZArith"),[0,c("BinInt"),0]]],0],wL=c(al),wM=c(al),wN=c(al),wO=c(al),wP=c(al),wQ=c(al),Ab=c("Micromega_plugin.Coq_micromega.M.ParseError"),BJ=c("Micromega_plugin.Coq_micromega.CsdpNotFound"),Ca=c(".csdp.cache"),Cb=c("csdp"),Cp=c(".lia.cache"),Cs=c(".nia.cache"),Cv=c(".nra.cache"),Cz=c(mU),CD=c(mU),CG=c("nra"),CK=c(mE),CM=c("nlia"),CS=c(hI),CV=c(hI),CY=c(hI),C4=[0,c("myred"),0],C6=c("RED"),C9=c("is_ground"),C$=c("ISGROUND"),Dc=c(mr),Dg=c(mr),Di=c(md),Dl=c("xlia"),Dn=c(hW),Dq=c("xnlia"),Ds=c("Nia"),Dv=c("xnra"),Dx=c("NRA"),DA=c("xnqa"),DC=c("NQA"),DF=c("sos_Z"),DH=c("Sos_Z"),DK=c("sos_Q"),DM=c("Sos_Q"),DP=c("sos_R"),DR=c("Sos_R"),DU=c("lra_Q"),DW=c("LRA_Q"),DZ=c("lra_R"),D1=c("LRA_R"),D4=c(m0),D8=c(m0),D_=c("PsatzR"),Eb=c(mt),Ef=c(mt),Eh=c("PsatzQ"),En=c("end_itlist"),Ep=c("tryfind"),Er=c("choose: completely undefined function"),EP=c(aJ),EO=c("strings_of_file: can't open "),EK=c(" expected"),Es=c("apply"),Eo=c(aK),Ej=[0,2],Ek=[0,10],Et=c("Micromega_plugin.Sos_lib.Noparse"),ER=c("Micromega_plugin.Sos_lib.TooDeep"),Fk=[0,1],HZ=[0,0,0],Iw=[0,0],Iu=[0,0],Iv=[0,0],Is=[0,1],Ip=[0,1],Iq=[0,2],Io=[0,0,0],Im=[0,1],In=[0,-1],Ir=[0,0,0],It=[0,0,0],Ik=c(mJ),Il=c(mc),Ib=c(hJ),Ic=c(hN),Id=c(hO),Ie=c(hM),If=c(aK),Ij=c(hR),Ig=c(ar),Ih=c(hL),Ii=c(hQ),H7=c(aK),H8=c(aJ),H9=c(mQ),H_=c(aJ),H$=c(hG),Ia=c(hV),H4=c(aJ),H5=c(ar),H6=c(ar),H2=c(m9),H3=c(aK),H0=c(aJ),H1=c(ar),HT=[0,0],HR=[0,0],HS=[0,0],HQ=[0,1],HM=[0,1],HN=[0,2],HL=[0,1],HO=[0,0,0,0],HP=[0,0,0,0],HU=[0,-1],HV=[0,0,0,0],HI=[0,0],HG=[0,0],HD=c(mJ),HE=c(mc),Hu=c(hJ),Hv=c(hN),Hw=c(hO),Hx=c(hM),Hy=c(aK),HC=c(hR),Hz=c(ar),HA=c(hL),HB=c(hQ),Hn=c(aK),Ho=c(aJ),Hp=c(ar),Hq=c(aJ),Hr=c(aJ),Hs=c(hG),Ht=c(hV),Hj=c(aJ),Hk=c(ar),Hl=c(ar),Hm=c(ar),Hh=c(ar),Hi=c(aK),Hg=[0,0,0,0],Hb=[0,0],Hc=[0,1],Ha=[0,0],Hd=[0,0],He=[0,1],Hf=[0,1],G8=[0,0],G9=c(mF),G_=[0,0],G$=c(mF),G7=c("diagonalize: non-square matrix"),G6=[0,0,0],G4=[0,0],G5=[0,0],G3=[0,-1],G2=c("choose_variable"),G1=[0,0],G0=[0,0],GZ=[0,0],GX=[0,c("_vendor+v8.10+32bit/coq/plugins/micromega/sos.ml"),533,12],GW=[0,1],GV=c("linear_program: An error occurred in the SDP solver"),GR=[0,1],GS=[0,1],GT=[0,0],GU=[0,0],GI=c(hJ),GJ=c(hN),GK=c(hO),GL=c(hM),GM=c(aK),GQ=c(hR),GN=c(ar),GO=c(hL),GP=c(hQ),Gn=[0,0,0],Gi=c("mkparser: unparsed input"),Ge=[0,10],F4=c("+"),F5=c(mw),FH=c(aK),FI=c(aJ),FJ=c(mQ),FK=c(aJ),FL=c(hG),FM=c(hV),FE=c(aJ),FF=c(ar),FG=c(ar),FC=c(m9),FD=c(aK),FA=c(aJ),FB=c(ar),Ft=[0,0],Fu=c(" - "),Fv=c(cT),Fr=c("<<0>>"),Fs=c(aK),Fw=c(">>"),Fx=c(cT),Fz=c(mw),Fy=c("<<"),Fo=[0,1],Fp=c(mG),Fm=c("1"),Fn=c(mG),Fl=c("^"),Fj=[0,0],Fi=[0,0],Fh=[0,0],Fg=[0,0],Ff=[0,1],Fb=[0,0],Fa=c("matrix_add: incompatible dimensions"),E$=[0,0],E9=[0,0],E8=[0,0],E7=[0,0],E6=[0,0],ET=[0,10],EU=[0,1],EV=[0,10],EW=[0,1],EX=[0,10],EY=[0,0],EZ=c("0.0"),E0=[0,1],E1=c(aK),E5=c(m5),E2=[0,0],E3=c("-0."),E4=c("0."),ES=c("Micromega_plugin.Sos.Sanity"),FX=c(mZ),F8=c("E"),F_=c(m5),Gj=c("}"),Gk=c(mS),Gl=c("{"),Go=c(mh),Gp=c("xVec"),Gs=c(aJ),Gu=c(ar),GA=c(ar),HJ=[0,-1];function
hY(c,f){var
h=f[2],i=f[1];if(i){var
j=b(d[40],i[1]);g(k[1],c,m$,j)}else
a(e[55],c,nd);a(e[55],c,na);if(h){var
l=b(d[40],h[1]);return g(k[1],c,nb,l)}return a(e[55],c,nc)}function
fA(b){var
c=b[1];if(c){var
e=b[2];if(e)return a(d[29],c[1],e[1])?[0,b]:0}return[0,b]}function
fB(c,b){var
f=b[2],g=b[1],h=c[2],i=c[1];function
e(d,c,b){if(c){var
e=c[1];return b?[0,a(d,e,b[1])]:c}return b?b:0}var
j=e(d[39],h,f);return fA([0,e(d[38],i,g),j])}function
dP(c){var
e=c[1];if(e){var
f=c[2];if(f){var
g=f[1],h=b(d[24],e[1]),i=b(d[22],g),j=a(d[4],i,h);return[0,a(d[1],j,ne)]}}return 0}function
hZ(f,e){var
b=dP(f),c=dP(e);return b?c?a(d[29],b[1],c[1]):1:0}function
fC(e,b){var
c=e[2],f=e[1];if(f){var
g=f[1];if(c){var
i=c[1],h=a(d[29],g,b);return h?a(d[29],b,i):h}return a(d[29],g,b)}return c?a(d[29],b,c[1]):1}aA(980,[0,hY,fB,dP,hZ,fC,fA],"Micromega_plugin__Itv");function
bo(a){return 0===a?1:0}function
bM(a){return a[1]}function
nf(a){return a[2]}function
P(a,b){if(a){var
c=a[1];return[0,c,P(a[2],b)]}return b}function
h0(a){switch(a){case
0:return 0;case
1:return 2;default:return 1}}function
fD(b,a){return b?[0,fD(b[1],a)]:a}function
h1(e,d,c){var
b=e,a=d;for(;;){if(b){var
f=b[1];if(a){var
b=f,a=a[2];continue}return c}return a?a[1]:c}}function
ba(c,a){if(a){var
d=a[1],e=ba(c,a[2]);return[0,b(c,d),e]}return 0}function
dQ(d,c,b){if(b){var
e=b[1];return a(d,e,dQ(d,c,b[2]))}return c}var
ng=[0];function
bp(a){return typeof
a==="number"?nh:0===a[0]?[1,bp(a[1])]:[0,a[1]]}function
co(b,a){if(typeof
b==="number")return typeof
a==="number"?ni:0===a[0]?[1,bp(a[1])]:[0,a[1]];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[1,bp(c)]:0===a[0]?[1,cU(c,a[1])]:[0,co(c,a[1])]}var
d=b[1];return typeof
a==="number"?[0,d]:0===a[0]?[0,co(d,a[1])]:[1,co(d,a[1])]}}function
cU(b,a){if(typeof
b==="number")return typeof
a==="number"?nj:0===a[0]?[0,bp(a[1])]:[1,bp(a[1])];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,bp(c)]:0===a[0]?[0,cU(c,a[1])]:[1,cU(c,a[1])]}var
d=b[1];return typeof
a==="number"?[1,bp(d)]:0===a[0]?[1,cU(d,a[1])]:[0,co(d,a[1])]}}function
cV(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[0,cV(a[1])]}function
cW(a){return typeof
a==="number"?0===a?nk:1:[0,[0,a[1]]]}function
cX(a){return typeof
a==="number"?a:[0,[1,a[1]]]}function
h2(a){return typeof
a==="number"?0:0===a[0]?[0,[1,[1,a[1]]]]:[0,[1,cV(a[1])]]}function
cp(b,a){if(typeof
b==="number")return typeof
a==="number"?0:1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,[1,c]]:0===a[0]?cX(cp(c,a[1])):cW(cp(c,a[1]))}var
d=b[1];return typeof
a==="number"?[0,cV(d)]:0===a[0]?cW(cY(d,a[1])):cX(cp(d,a[1]))}}function
cY(b,a){if(typeof
b==="number")return 1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,cV(c)]:0===a[0]?cW(cY(c,a[1])):cX(cp(c,a[1]))}var
d=b[1];return typeof
a==="number"?h2(d):0===a[0]?cX(cY(d,a[1])):cW(cY(d,a[1]))}}function
fE(c,b){var
a=cp(c,b);return typeof
a==="number"?0:a[1]}function
fF(b,a){return typeof
b==="number"?a:0===b[0]?co(a,[1,fF(b[1],a)]):[1,fF(b[1],a)]}function
dR(a,h,g){var
d=h,c=g;for(;;)if(typeof
c==="number")return b(a,d);else{if(0===c[0]){var
e=c[1];return b(a,dR(a,dR(a,d,e),e))}var
f=c[1],d=dR(a,d,f),c=f;continue}}function
cZ(a){return typeof
a==="number"?nl:0===a[0]?[0,cZ(a[1])]:[0,cZ(a[1])]}function
h3(h,g,f){var
c=h,b=g,a=f;for(;;)if(typeof
b==="number")return typeof
a==="number"?c:1;else{if(0===b[0]){var
d=b[1];if(typeof
a==="number")return 2;else{if(0===a[0]){var
b=d,a=a[1];continue}var
c=2,b=d,a=a[1];continue}}var
e=b[1];if(typeof
a==="number")return 2;else{if(0===a[0]){var
c=1,b=e,a=a[1];continue}var
b=e,a=a[1];continue}}}var
nm=0;function
h4(a,b){return h3(nm,a,b)}function
fG(j,i,h){var
c=j,b=i,a=h;for(;;){if(c){var
d=c[1];if(typeof
b==="number")return 0;else{if(0===b[0]){var
e=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
f=a[1];switch(h4(e,f)){case
0:return b;case
1:var
c=d,a=b,b=fE(f,e);continue;default:var
c=d,b=fE(e,f);continue}}var
c=d,a=a[1];continue}}var
g=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=d,b=g;continue}return[1,fG(d,g,a[1])]}}}return 0}}function
nn(b,a){var
c=cZ(a);return fG(fD(cZ(b),c),b,a)}function
h5(a){return a?bp(h5(a[1])):0}var
u=[0,bp,co,cU,cV,cW,cX,h2,cp,cY,fE,fF,dR,cZ,h3,h4,fG,nn,h5],h6=[0,function(a){return a?[0,b(u[18],a[1])]:0}];function
dS(b,d,c){if(typeof
c==="number")return d;else{if(0===c[0]){var
e=dS(b,d,c[1]);return a(b,d,a(b,e,e))}var
f=dS(b,d,c[1]);return a(b,f,f)}}function
fH(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[1,[1,a[1]]]}function
h7(a){return typeof
a==="number"?no:0===a[0]?[0,[0,a[1]]]:[1,b(u[4],a[1])]}function
h8(a){return typeof
a==="number"?np:0===a[0]?[0,b(u[4],a[1])]:[1,[0,a[1]]]}function
b1(c,a){if(typeof
c==="number")return typeof
a==="number"?0:0===a[0]?[1,[1,a[1]]]:[1,b(u[4],a[1])];else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?[0,[1,d]]:0===a[0]?fH(b1(d,a[1])):h7(b1(d,a[1]))}var
e=c[1];return typeof
a==="number"?[0,b(u[4],e)]:0===a[0]?h8(b1(e,a[1])):fH(b1(e,a[1]))}}function
bN(c,b){if(typeof
c==="number")return b;else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?c:0===b[0]?[0,a(u[2],d,b[1])]:b1(d,b[1])}var
e=c[1];return typeof
b==="number"?c:0===b[0]?b1(b[1],e):[1,a(u[2],e,b[1])]}}function
b2(a){return typeof
a==="number"?0:0===a[0]?[1,a[1]]:[0,a[1]]}function
dT(b,a){return bN(b,b2(a))}function
bO(c,b){if(typeof
c==="number")return 0;else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?0:0===b[0]?[0,a(u[11],d,b[1])]:[1,a(u[11],d,b[1])]}var
e=c[1];return typeof
b==="number"?0:0===b[0]?[1,a(u[11],e,b[1])]:[0,a(u[11],e,b[1])]}}function
h9(b){function
c(a){return bO(b,a)}return a(u[12],c,nq)}function
nr(c,a){if(typeof
a==="number")return ns;else{if(0===a[0]){var
d=a[1];return b(h9(c),d)}return 0}}function
c0(c,b){if(typeof
c==="number")return typeof
b==="number"?0:0===b[0]?1:2;else{if(0===c[0]){var
d=c[1];if(typeof
b!=="number"&&0===b[0])return a(u[15],d,b[1]);return 2}var
e=c[1];if(typeof
b!=="number"&&1===b[0])return h0(a(u[15],e,b[1]));return 1}}function
h_(b,a){return 2<=c0(b,a)?0:1}function
fI(b,a){return 1===c0(b,a)?1:0}function
nt(b,a){return 2<=c0(b,a)?1:0}function
nu(b,a){return 1===c0(b,a)?a:b}function
dU(a){if(typeof
a!=="number"&&1===a[0])return[0,a[1]];return a}function
nv(a){if(typeof
a!=="number"&&0===a[0])return[0,a[1]];return 0}function
nw(a){return a?[0,b(u[18],a[1])]:0}function
nx(a){return a?[0,a[1]]:0}function
b3(b,a){if(typeof
b==="number")return h_(ny,a)?nz:nA;else{if(0===b[0]){var
e=b3(b[1],a),f=e[1],c=bN(bO(nC,e[2]),nB);if(fI(c,a))return[0,bO(nD,f),c];var
i=dT(c,a);return[0,bN(bO(nF,f),nE),i]}var
g=b3(b[1],a),h=g[1],d=bO(nG,g[2]);if(fI(d,a))return[0,bO(nH,h),d];var
j=dT(d,a);return[0,bN(bO(nJ,h),nI),j]}}function
h$(b,a){if(typeof
b==="number")return nK;else{if(0===b[0]){var
c=b[1];if(typeof
a==="number")return nL;else{if(0===a[0])return b3(c,a);var
d=b3(c,[0,a[1]]),e=d[2],f=d[1];if(typeof
e==="number")return[0,b2(f),0];var
l=bN(a,e);return[0,b2(bN(f,nM)),l]}}var
g=b[1];if(typeof
a==="number")return nN;else{if(0===a[0]){var
h=b3(g,a),i=h[2],j=h[1];if(typeof
i==="number")return[0,b2(j),0];var
m=dT(a,i);return[0,b2(bN(j,nO)),m]}var
k=b3(g,[0,a[1]]),n=k[1];return[0,n,b2(k[2])]}}}function
nP(b,a){return h$(b,a)[1]}var
n=[0,fH,h7,h8,b1,bN,b2,dT,bO,h9,nr,c0,h_,fI,nt,nu,dU,nv,nw,nx,b3,h$,nP,function(c,b){if(typeof
c==="number")return dU(b);else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?dU(c):0===b[0]?[0,a(u[17],d,b[1])]:[0,a(u[17],d,b[1])]}var
e=c[1];return typeof
b==="number"?dU(c):0===b[0]?[0,a(u[17],e,b[1])]:[0,a(u[17],e,b[1])]}}];function
aL(c,b){return 0===a(n[11],c,b)?1:0}function
nQ(a){return[0,a]}function
nR(a){return[0,a]}function
fJ(d,f,e){var
c=f,b=e;for(;;)switch(c[0]){case
0:var
g=c[1];return 0===b[0]?a(d,g,b[1]):0;case
1:var
h=c[2],i=c[1];if(1===b[0]){var
j=b[2];if(0===a(u[15],i,b[1])){var
c=h,b=j;continue}return 0}return 0;default:var
k=c[3],l=c[2],m=c[1];if(2===b[0]){var
n=b[3],o=b[1];if(0===a(u[15],l,b[2])){if(fJ(d,m,o)){var
c=k,b=n;continue}return 0}return 0}return 0}}function
ae(c,b){switch(b[0]){case
0:return b;case
1:var
d=b[2];return[1,a(u[2],c,b[1]),d];default:return[1,c,b]}}function
ia(a,c){return typeof
a==="number"?c:0===a[0]?[1,[1,a[1]],c]:[1,b(u[4],a[1]),c]}function
U(f,e,b,d,c){switch(b[0]){case
0:return a(e,b[1],f)?ae(0,c):[2,b,d,c];case
1:return[2,b,d,c];default:var
g=b[2],h=b[1];return fJ(e,b[3],[0,f])?[2,h,a(u[2],g,d),c]:[2,b,d,c]}}function
ib(c,b,a){return[2,[0,b],a,[0,c]]}function
ic(b,a){return ib(b,a,0)}function
as(c,a){switch(a[0]){case
0:return[0,b(c,a[1])];case
1:var
d=a[1];return[1,d,as(c,a[2])];default:var
e=a[2],f=a[1],g=as(c,a[3]);return[2,as(c,f),e,g]}}function
bP(d,b,c){switch(b[0]){case
0:return[0,a(d,b[1],c)];case
1:var
e=b[1];return[1,e,bP(d,b[2],c)];default:var
f=b[2],g=b[1];return[2,g,f,bP(d,b[3],c)]}}function
cq(d,b,c){switch(b[0]){case
0:return[0,a(d,b[1],c)];case
1:var
e=b[1];return[1,e,cq(d,b[2],c)];default:var
f=b[2],g=b[1];return[2,g,f,cq(d,b[3],c)]}}function
c1(g,f,e,c,d){switch(d[0]){case
0:return ae(c,bP(g,e,d[1]));case
1:var
i=d[2],m=d[1],h=a(n[4],m,c);return typeof
h==="number"?ae(c,a(f,i,e)):0===h[0]?ae(c,a(f,[1,h[1],i],e)):ae(m,c1(g,f,e,h[1],i));default:var
j=d[3],k=d[2],l=d[1];return typeof
c==="number"?[2,l,k,a(f,j,e)]:0===c[0]?[2,l,k,c1(g,f,e,[1,c[1]],j)]:[2,l,k,c1(g,f,e,b(u[4],c[1]),j)]}}function
c2(h,g,f,e,c,d){switch(d[0]){case
0:var
p=d[1];return ae(c,bP(h,as(g,e),p));case
1:var
j=d[2],o=d[1],i=a(n[4],o,c);return typeof
i==="number"?ae(c,a(f,j,e)):0===i[0]?ae(c,a(f,[1,i[1],j],e)):ae(o,c2(h,g,f,e,i[1],j));default:var
k=d[3],l=d[2],m=d[1];return typeof
c==="number"?[2,m,l,a(f,k,e)]:0===c[0]?[2,m,l,c2(h,g,f,e,[1,c[1]],k)]:[2,m,l,c2(h,g,f,e,b(u[4],c[1]),k)]}}function
fK(f,g,j,d,e,c){switch(c[0]){case
0:return[2,d,e,c];case
1:var
k=c[2],h=c[1];return typeof
h==="number"?[2,d,e,k]:0===h[0]?[2,d,e,[1,[1,h[1]],k]]:[2,d,e,[1,b(u[4],h[1]),k]];default:var
l=c[3],m=c[2],o=c[1],i=a(n[4],m,e);return typeof
i==="number"?U(f,g,a(j,o,d),m,l):0===i[0]?U(f,g,a(j,[2,o,i[1],[0,f]],d),e,l):U(f,g,fK(f,g,j,d,i[1],o),m,l)}}function
fL(g,f,h,k,d,e,c){switch(c[0]){case
0:return[2,as(f,d),e,c];case
1:var
l=c[2],i=c[1];if(typeof
i==="number")return[2,as(f,d),e,l];else{if(0===i[0]){var
q=[1,[1,i[1]],l];return[2,as(f,d),e,q]}var
r=[1,b(u[4],i[1]),l];return[2,as(f,d),e,r]}default:var
m=c[3],o=c[2],p=c[1],j=a(n[4],o,e);return typeof
j==="number"?U(g,h,a(k,p,d),o,m):0===j[0]?U(g,h,a(k,[2,p,j[1],[0,g]],d),e,m):U(g,h,fL(g,f,h,k,d,j[1],p),o,m)}}function
ab(c,e,d,f,g){switch(g[0]){case
0:return bP(e,f,g[1]);case
1:var
r=g[2],s=g[1];return c1(e,function(a,b){return ab(c,e,d,a,b)},r,s,f);default:var
h=g[3],j=g[2],i=g[1];switch(f[0]){case
0:return[2,i,j,bP(e,h,f[1])];case
1:var
m=f[2],k=f[1];return typeof
k==="number"?[2,i,j,ab(c,e,d,m,h)]:0===k[0]?[2,i,j,ab(c,e,d,[1,[1,k[1]],m],h)]:[2,i,j,ab(c,e,d,[1,b(u[4],k[1]),m],h)];default:var
o=f[3],p=f[2],q=f[1],l=a(n[4],p,j);if(typeof
l==="number"){var
t=ab(c,e,d,o,h);return U(c,d,ab(c,e,d,q,i),p,t)}else{if(0===l[0]){var
v=l[1],w=ab(c,e,d,o,h);return U(c,d,ab(c,e,d,[2,q,v,[0,c]],i),j,w)}var
x=l[1],y=ab(c,e,d,o,h);return U(c,d,fK(c,d,function(a,b){return ab(c,e,d,a,b)},i,x,q),p,y)}}}}function
I(d,f,g,c,e,h,i){switch(i[0]){case
0:return cq(g,h,i[1]);case
1:var
t=i[2],v=i[1];return c2(f,c,function(a,b){return I(d,f,g,c,e,a,b)},t,v,h);default:var
j=i[3],l=i[2],k=i[1];switch(h[0]){case
0:var
w=h[1],x=bP(f,as(c,j),w);return[2,as(c,k),l,x];case
1:var
p=h[2],m=h[1];if(typeof
m==="number"){var
y=I(d,f,g,c,e,p,j);return[2,as(c,k),l,y]}else{if(0===m[0]){var
z=I(d,f,g,c,e,[1,[1,m[1]],p],j);return[2,as(c,k),l,z]}var
A=I(d,f,g,c,e,[1,b(u[4],m[1]),p],j);return[2,as(c,k),l,A]}default:var
q=h[3],r=h[2],s=h[1],o=a(n[4],r,l);if(typeof
o==="number"){var
B=I(d,f,g,c,e,q,j);return U(d,e,I(d,f,g,c,e,s,k),r,B)}else{if(0===o[0]){var
C=o[1],D=I(d,f,g,c,e,q,j);return U(d,e,I(d,f,g,c,e,[2,s,C,[0,d]],k),l,D)}var
E=o[1],F=I(d,f,g,c,e,q,j);return U(d,e,fL(d,c,e,function(a,b){return I(d,f,g,c,e,a,b)},k,E,s),r,F)}}}}function
c3(f,e,d,b,c){switch(b[0]){case
0:return[0,a(e,b[1],c)];case
1:var
g=b[1];return ae(g,c3(f,e,d,b[2],c));default:var
h=b[2],i=b[1],j=c3(f,e,d,b[3],c);return U(f,d,c3(f,e,d,i,c),h,j)}}function
c4(d,g,f,c,e,b){return a(c,b,d)?[0,d]:a(c,b,g)?e:c3(d,f,c,e,b)}function
bq(f,j,i,e,g,d,c,h){switch(h[0]){case
0:return ae(c,c4(f,j,i,e,d,h[1]));case
1:var
l=h[2],q=h[1],k=a(n[4],q,c);return typeof
k==="number"?ae(c,a(g,l,d)):0===k[0]?ae(c,a(g,[1,k[1],l],d)):ae(q,bq(f,j,i,e,g,d,k[1],l));default:var
m=h[3],o=h[2],p=h[1];if(typeof
c==="number"){var
r=a(g,m,d);return U(f,e,bq(f,j,i,e,g,d,0,p),o,r)}else{if(0===c[0]){var
s=bq(f,j,i,e,g,d,[1,c[1]],m);return U(f,e,bq(f,j,i,e,g,d,c,p),o,s)}var
t=bq(f,j,i,e,g,d,b(u[4],c[1]),m);return U(f,e,bq(f,j,i,e,g,d,c,p),o,t)}}}function
am(a,e,f,d,c,g,h){switch(h[0]){case
0:return c4(a,e,d,c,g,h[1]);case
1:var
q=h[2],r=h[1];return bq(a,e,d,c,function(b,g){return am(a,e,f,d,c,b,g)},q,r,g);default:var
i=h[3],m=h[2],k=h[1];switch(g[0]){case
0:return c4(a,e,d,c,h,g[1]);case
1:var
l=g[2],j=g[1],s=typeof
j==="number"?am(a,e,f,d,c,l,i):0===j[0]?am(a,e,f,d,c,[1,[1,j[1]],l],i):am(a,e,f,d,c,[1,b(u[4],j[1]),l],i);return U(a,c,am(a,e,f,d,c,g,k),m,s);default:var
n=g[3],o=g[2],p=g[1],t=am(a,e,f,d,c,n,i),v=0,w=bq(a,e,d,c,function(b,g){return am(a,e,f,d,c,b,g)},i,v,p),x=am(a,e,f,d,c,ae(0,n),k),y=am(a,e,f,d,c,p,k),z=U(a,c,w,o,t);return ab(a,f,c,U(a,c,ab(a,f,c,U(a,c,y,o,[0,a]),x),m,[0,a]),z)}}}function
c5(b,e,g,f,c,d){switch(d[0]){case
0:var
h=d[1];return[0,a(f,h,h)];case
1:var
l=d[1];return[1,l,c5(b,e,g,f,c,d[2])];default:var
i=d[3],j=d[2],k=d[1],m=am(b,e,g,f,c,k,ae(0,c4(b,e,f,c,i,a(g,e,e)))),n=c5(b,e,g,f,c,i);return U(b,c,ab(b,g,c,U(b,c,c5(b,e,g,f,c,k),j,[0,b]),m),j,n)}}function
id(c,b,a){return ia(a,ic(c,b))}function
c6(h,g,f,e,d,c,n,a,m){var
j=n,i=m;for(;;)if(typeof
i==="number")return b(c,am(h,g,f,e,d,j,a));else{if(0===i[0]){var
k=i[1];return b(c,am(h,g,f,e,d,c6(h,g,f,e,d,c,c6(h,g,f,e,d,c,j,a,k),a,k),a))}var
l=i[1],j=c6(h,g,f,e,d,c,j,a,l),i=l;continue}}function
ie(h,a,g,f,e,d,c,b){return b?c6(h,a,g,f,e,d,[0,a],c,b[1]):[0,a]}function
Y(a,f,c,g,e,d,b,h){switch(h[0]){case
0:return[0,h[1]];case
1:return id(a,f,h[1]);case
2:var
i=h[2],j=h[1];if(5===j[0]){var
m=Y(a,f,c,g,e,d,b,j[1]);return I(a,c,e,d,b,Y(a,f,c,g,e,d,b,i),m)}if(5===i[0]){var
l=Y(a,f,c,g,e,d,b,i[1]);return I(a,c,e,d,b,Y(a,f,c,g,e,d,b,j),l)}var
k=Y(a,f,c,g,e,d,b,i);return ab(a,c,b,Y(a,f,c,g,e,d,b,j),k);case
3:var
n=h[1],o=Y(a,f,c,g,e,d,b,h[2]);return I(a,c,e,d,b,Y(a,f,c,g,e,d,b,n),o);case
4:var
p=h[1],q=Y(a,f,c,g,e,d,b,h[2]);return am(a,f,c,g,b,Y(a,f,c,g,e,d,b,p),q);case
5:return as(d,Y(a,f,c,g,e,d,b,h[1]));default:var
r=h[2],s=Y(a,f,c,g,e,d,b,h[1]);return ie(a,f,c,g,b,function(a){return a},s,r)}}function
br(c,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,b(c,a[1])];case
2:var
d=a[1],e=br(c,a[2]);return[2,br(c,d),e];case
3:var
f=a[1],g=br(c,a[2]);return[3,br(c,f),g];case
4:return[4,br(c,a[1])];case
5:var
h=a[2],i=a[1],j=br(c,a[3]);return[5,br(c,i),h,j]}return a}function
c7(d,f,e){var
b=f,c=e;for(;;){if(typeof
b!=="number")switch(b[0]){case
1:return a(d,c,b[2]);case
2:var
g=b[1],h=c7(d,b[2],c),b=g,c=h;continue;case
3:var
i=b[1],j=c7(d,b[2],c),b=i,c=j;continue;case
4:var
b=b[1];continue;case
5:var
k=b[1],l=c7(d,b[3],c),b=k,c=l;continue}return c}}function
ig(b,a){return b?[0,b[1],a]:a}function
fM(a){if(typeof
a!=="number"&&5===a[0]){var
b=a[2];return ig(b,fM(a[3]))}return 0}function
b4(b){var
a=b;for(;;){if(typeof
a!=="number")switch(a[0]){case
1:return[0,a[2],0];case
2:var
c=a[1],d=b4(a[2]);return P(b4(c),d);case
3:var
e=a[1],f=b4(a[2]);return P(b4(e),f);case
4:var
a=a[1];continue;case
5:var
g=a[1],h=b4(a[3]);return P(b4(g),h)}return 0}}function
a1(c,a){if(typeof
a==="number")return 0===a?0:1;else
switch(a[0]){case
0:return[0,a[1]];case
1:var
d=a[2];return[1,b(c,a[1]),d];case
2:var
e=a[1],f=a1(c,a[2]);return[2,a1(c,e),f];case
3:var
g=a[1],h=a1(c,a[2]);return[3,a1(c,g),h];case
4:return[4,a1(c,a[1])];default:var
i=a[2],j=a[1],k=a1(c,a[3]);return[5,a1(c,j),i,k]}}var
b5=0;function
dV(e,d,c,f){if(f){var
h=f[2],g=f[1],i=a(d,c[1],g[1]);if(i){if(b(e,i[1]))return 0;var
j=dV(e,d,c,h);return j?[0,[0,g,j[1]]]:0}var
k=dV(e,d,c,h);return k?[0,[0,g,k[1]]]:0}var
l=a(d,c[1],c[1]);return l?b(e,l[1])?0:[0,[0,c,0]]:[0,[0,c,0]]}function
ih(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=dV(g,f,a[1],b);if(c){var
a=h,b=c[1];continue}return 0}return[0,b]}}function
ii(e,d,c,a){var
b=0;return dQ(function(f,a){var
b=ih(e,d,c,f);return b?[0,b[1],a]:a},b,a)}function
c8(d,c,a,b){if(a){var
e=a[2],f=ii(d,c,a[1],b);return P(c8(d,c,e,b),f)}return b5}function
aC(d,c,f,e,q,p){var
b=q,g=p;for(;;)if(typeof
g==="number")return 0===g?b?b5:b6:b?b6:b5;else
switch(g[0]){case
0:return b6;case
1:var
h=g[2],i=g[1];return b?a(f,i,h):a(e,i,h);case
2:var
j=g[2],k=g[1];if(b){var
r=aC(d,c,f,e,b,j);return P(aC(d,c,f,e,b,k),r)}var
s=aC(d,c,f,e,b,j);return c8(d,c,aC(d,c,f,e,b,k),s);case
3:var
l=g[2],m=g[1];if(b){var
t=aC(d,c,f,e,b,l);return c8(d,c,aC(d,c,f,e,b,m),t)}var
u=aC(d,c,f,e,b,l);return P(aC(d,c,f,e,b,m),u);case
4:var
v=g[1],b=bo(b),g=v;continue;default:var
n=g[3],o=g[1];if(b){var
w=aC(d,c,f,e,b,n);return c8(d,c,aC(d,c,f,e,bo(b),o),w)}var
x=aC(d,c,f,e,b,n);return P(aC(d,c,f,e,bo(b),o),x)}}function
dW(e,d,c,g){if(g){var
j=g[2],f=g[1],k=a(d,c[1],f[1]);if(k){if(b(e,k[1]))return[1,[0,c[2],[0,f[2],0]]];var
h=dW(e,d,c,j);return 0===h[0]?[0,[0,f,h[1]]]:[1,h[1]]}var
i=dW(e,d,c,j);return 0===i[0]?[0,[0,f,i[1]]]:[1,i[1]]}var
l=a(d,c[1],c[1]);return l?b(e,l[1])?[1,[0,c[2],0]]:[0,[0,c,0]]:[0,[0,c,0]]}function
ij(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=dW(g,f,a[1],b);if(0===c[0]){var
a=h,b=c[1];continue}return[1,c[1]]}return[0,b]}}function
ik(g,f,e,a){return dQ(function(h,b){var
c=b[2],d=b[1],a=ij(g,f,e,h);return 0===a[0]?[0,[0,a[1],d],c]:[0,d,P(c,a[1])]},nS,a)}function
c9(d,c,a,b){if(a){var
g=a[1],e=c9(d,c,a[2],b),h=e[2],i=e[1],f=ik(d,c,g,b),j=f[1],k=P(h,f[2]);return[0,P(i,j),k]}return[0,b5,0]}function
bs(e,d,g,f,F,E){var
b=F,c=E;for(;;)if(typeof
c==="number")return 0===c?b?[0,b5,0]:[0,b6,0]:b?[0,b6,0]:[0,b5,0];else
switch(c[0]){case
0:return[0,b6,0];case
1:var
h=c[2],i=c[1],G=0,H=b?a(g,i,h):a(f,i,h);return[0,H,G];case
2:var
I=c[2],j=bs(e,d,g,f,b,c[1]),k=j[2],l=j[1],m=bs(e,d,g,f,b,I),n=m[2],o=m[1];if(b){var
J=P(k,n);return[0,P(l,o),J]}var
p=c9(e,d,l,o),K=p[1];return[0,K,P(k,P(n,p[2]))];case
3:var
L=c[2],q=bs(e,d,g,f,b,c[1]),r=q[2],s=q[1],t=bs(e,d,g,f,b,L),u=t[2],v=t[1];if(b){var
w=c9(e,d,s,v),M=w[1];return[0,M,P(r,P(u,w[2]))]}var
N=P(r,u);return[0,P(s,v),N];case
4:var
O=c[1],b=bo(b),c=O;continue;default:var
Q=c[3],R=c[1],x=bs(e,d,g,f,bo(b),R),y=x[2],z=x[1],A=bs(e,d,g,f,b,Q),B=A[2],C=A[1];if(b){var
D=c9(e,d,z,C),S=D[1];return[0,S,P(y,P(B,D[2]))]}var
T=P(y,B);return[0,P(z,C),T]}}function
il(f,e,d){var
c=e,b=d;for(;;){if(c){var
g=c[2],h=c[1];if(b){var
i=b[2];if(a(f,h,b[1])){var
c=g,b=i;continue}return 0}return 0}return 1}}function
dX(g,f,e,d,c,b,a){return il(c,aC(g,f,e,d,1,b),a)}function
fN(d,c,b){return bo(a(d,c,b))}function
fO(f,e,c,b){var
d=a(e,c,b);return d?fN(f,c,b):d}function
im(b,a){switch(b){case
0:return nT;case
1:return 1===a?nU:0===a?nV:0;case
2:return 1===a?0:[0,a];default:return 1===a?0:0===a?nW:nX}}function
io(b,a){switch(b){case
0:return[0,a];case
1:return 0===a?nY:0;case
2:return 1===a?0:nZ;default:return 1===a?0:0===a?n0:[0,a]}}function
cr(c,a){return a?b(c,a[1]):0}function
cs(d,c,b){if(c){var
e=c[1];return b?a(d,e,b[1]):0}return 0}function
ip(g,f,e,d,c,b,a){var
h=a[1];return 0===a[2]?[0,[0,am(g,f,e,d,c,b,h),0]]:0}function
iq(g,f,e,d,c,b,a){var
h=b[1],i=a[1],j=im(b[2],a[2]);return cr(function(a){return[0,[0,am(g,f,e,d,c,h,i),a]]},j)}function
c_(e,d,c,b,a){var
f=b[1],g=a[1],h=io(b[2],a[2]);return cr(function(a){return[0,[0,ab(e,d,c,f,g),a]]},h)}function
bQ(a,f,d,e,c,h,g,b){if(typeof
b==="number")return[0,[0,[0,a],0]];else
switch(b[0]){case
0:return[0,h1(b[1],g,[0,[0,a],0])];case
1:return[0,[0,c5(a,f,d,e,c,b[1]),3]];case
2:var
j=b[1],k=bQ(a,f,d,e,c,h,g,b[2]);return cr(function(b){return ip(a,f,d,e,c,j,b)},k);case
3:var
l=b[1],m=bQ(a,f,d,e,c,h,g,b[2]),n=bQ(a,f,d,e,c,h,g,l);return cs(function(b,g){return iq(a,f,d,e,c,b,g)},n,m);case
4:var
o=b[1],p=bQ(a,f,d,e,c,h,g,b[2]),q=bQ(a,f,d,e,c,h,g,o);return cs(function(b,e){return c_(a,d,c,b,e)},q,p);default:var
i=b[1];return fO(c,h,a,i)?[0,[0,[0,i],2]]:0}}function
c$(b,d,f,e){var
g=e[1],h=e[2];if(0===g[0]){var
c=g[1];switch(h){case
0:return fN(d,c,b);case
1:return a(d,c,b);case
2:return a(f,c,b);default:return fO(d,f,c,b)}}return 0}function
dY(c,i,h,g,b,a,f,e){var
d=bQ(c,i,h,g,b,a,f,e);return d?c$(c,b,a,d[1]):0}function
ir(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=Y(e,j,d,i,c,b,a,h[1]),g=Y(e,j,d,i,c,b,a,k);switch(l){case
0:var
m=[0,[0,I(e,d,c,b,a,g,f),2],0];return[0,[0,I(e,d,c,b,a,f,g),2],m];case
1:return[0,[0,I(e,d,c,b,a,f,g),0],0];case
2:return[0,[0,I(e,d,c,b,a,f,g),2],0];case
3:return[0,[0,I(e,d,c,b,a,g,f),2],0];case
4:return[0,[0,I(e,d,c,b,a,f,g),3],0];default:return[0,[0,I(e,d,c,b,a,g,f),3],0]}}function
fP(i,h,g,f,e,d,c,b,a){var
j=ir(i,h,g,f,e,d,c,b);return ba(function(b){return[0,[0,b,a],0]},j)}function
is(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=Y(e,j,d,i,c,b,a,h[1]),g=Y(e,j,d,i,c,b,a,k);switch(l){case
0:return[0,[0,I(e,d,c,b,a,f,g),0],0];case
1:var
m=[0,[0,I(e,d,c,b,a,g,f),2],0];return[0,[0,I(e,d,c,b,a,f,g),2],m];case
2:return[0,[0,I(e,d,c,b,a,g,f),3],0];case
3:return[0,[0,I(e,d,c,b,a,f,g),3],0];case
4:return[0,[0,I(e,d,c,b,a,g,f),2],0];default:return[0,[0,I(e,d,c,b,a,f,g),2],0]}}function
fQ(i,h,g,f,e,d,c,b,a){var
j=is(i,h,g,f,e,d,c,b);return ba(function(b){return[0,[0,b,a],0]},j)}function
dZ(f,e){var
d=f,c=e;for(;;)switch(c[0]){case
0:return[0,c[1]];case
1:var
g=c[2],d=a(u[2],c[1],d),c=g;continue;default:var
h=c[3],i=c[2],j=c[1],k=dZ(b(u[1],d),h);return[2,[4,dZ(d,j),[6,[1,d],[0,i]]],k]}}function
it(a){return dZ(0,a)}function
a2(c,a){switch(a[0]){case
0:return[0,b(c,a[1])];case
1:return[1,a[1]];case
2:var
d=a[1],e=a2(c,a[2]);return[2,a2(c,d),e];case
3:var
f=a[1],g=a2(c,a[2]);return[3,a2(c,f),g];case
4:var
h=a[1],i=a2(c,a[2]);return[4,a2(c,h),i];case
5:return[5,a2(c,a[1])];default:var
j=a[2];return[6,a2(c,a[1]),j]}}function
d0(b,a){var
c=a[2],d=a[1],e=a2(b,a[3]);return[0,a2(b,d),c,e]}function
iu(q,h,f,g,c){if(typeof
c!=="number")switch(c[0]){case
1:var
m=c[1];if(0===m[0]){var
n=m[1];return a(g,q,n)?0:[5,a(f,n,n)]}return[1,m];case
3:var
b=c[2],d=c[1];if(typeof
d==="number")return 0;else
switch(d[0]){case
3:var
i=d[2],j=d[1];if(typeof
j!=="number"&&5===j[0]){var
s=j[1];return typeof
b==="number"?0:5===b[0]?[3,[5,a(f,b[1],s)],i]:c}if(typeof
i!=="number"&&5===i[0]){var
r=i[1];return typeof
b==="number"?0:5===b[0]?[3,[5,a(f,b[1],r)],j]:c}return typeof
b==="number"?0:5===b[0]?a(g,h,b[1])?d:[3,d,b]:c;case
5:var
e=d[1];if(typeof
b==="number")return 0;else
switch(b[0]){case
3:var
k=b[2],l=b[1];if(typeof
l!=="number"&&5===l[0])return[3,[5,a(f,e,l[1])],k];if(typeof
k!=="number"&&5===k[0])return[3,[5,a(f,e,k[1])],l];return a(g,h,e)?b:[3,d,b];case
4:return[4,[3,[5,e],b[1]],[3,[5,e],b[2]]];case
5:return[5,a(f,e,b[1])];default:return a(g,h,e)?b:[3,d,b]}default:return typeof
b==="number"?0:5===b[0]?a(g,h,b[1])?d:[3,d,b]:c}case
4:var
o=c[2],p=c[1];return typeof
p==="number"?o:typeof
o==="number"?p:[4,p,o]}return c}var
n1=[0];function
n2(a){return a[1]}function
n3(a){return a[2]}function
at(c,b){var
d=a(n[8],b[1],[0,c[2]]);return aL(a(n[8],c[1],[0,b[2]]),d)}function
da(c,b){var
d=a(n[8],b[1],[0,c[2]]),e=a(n[8],c[1],[0,b[2]]);return a(n[12],e,d)}function
aM(c,b){var
d=a(u[11],c[2],b[2]),e=a(n[8],b[1],[0,c[2]]),f=a(n[8],c[1],[0,b[2]]);return[0,a(n[5],f,e),d]}function
aT(c,b){var
d=a(u[11],c[2],b[2]);return[0,a(n[8],c[1],b[1]),d]}function
bt(a){var
c=a[2];return[0,b(n[6],a[1]),c]}function
bR(b,a){return aM(b,bt(a))}function
fR(b){var
a=b[1];return typeof
a==="number"?n4:0===a[0]?[0,[0,b[2]],a[1]]:[0,[1,b[2]],a[1]]}function
fS(a,b){return dS(aT,a,b)}function
fT(b,a){return typeof
a==="number"?n5:0===a[0]?fS(b,a[1]):fR(fS(b,a[1]))}function
n6(e,d,c){var
a=d,b=c;for(;;)if(typeof
a==="number")return e;else{if(0===a[0])return a[1];var
f=a[3],g=a[2],h=a[1];if(typeof
b==="number")return g;else{if(0===b[0]){var
a=f,b=b[1];continue}var
a=h,b=b[1];continue}}}function
ct(b,a,c){return typeof
a==="number"?[0,c]:0===a[0]?[1,0,b,ct(b,a[1],c)]:[1,ct(b,a[1],c),b,0]}function
d1(d,a,b,c){if(typeof
c==="number")return ct(d,a,b);else{if(0===c[0]){var
g=c[1];return typeof
a==="number"?[0,b]:0===a[0]?[1,0,g,ct(d,a[1],b)]:[1,ct(d,a[1],b),g,0]}var
e=c[3],h=c[2],f=c[1];return typeof
a==="number"?[1,f,b,e]:0===a[0]?[1,f,h,d1(d,a[1],b,e)]:[1,d1(d,a[1],b,f),h,e]}}function
bb(c){switch(c[0]){case
0:return[0,c[1]];case
1:return 0;case
2:var
d=c[1],e=bb(c[2]),f=bb(d);return cs(function(c,b){return[0,a(n[5],c,b)]},f,e);case
3:var
g=c[1],h=bb(c[2]),i=bb(g);return cs(function(c,b){return[0,a(n[7],c,b)]},i,h);case
4:var
j=c[1],k=bb(c[2]),l=bb(j);return cs(function(c,b){return[0,a(n[8],c,b)]},l,k);case
5:var
m=bb(c[1]);return cr(function(a){return[0,b(n[6],a)]},m);default:var
o=c[2],p=bb(c[1]);return cr(function(c){var
d=b(n[19],o);return[0,a(n[10],c,d)]},p)}}var
n7=n[12],n8=n[8],n9=n[5],n$=0;function
iv(a,b){return dY(n$,n_,n9,n8,aL,n7,a,b)}var
oa=n[6],ob=n[7],oc=n[5],od=0;function
an(a,b){return I(od,oc,ob,oa,aL,a,b)}var
oe=n[5],of=0;function
a3(a,b){return ab(of,oe,aL,a,b)}var
og=n[6],oh=n[7],oi=n[8],oj=n[5],ol=0;function
cu(a){return Y(ol,ok,oj,oi,oh,og,aL,a)}function
iw(c){var
d=c[3],e=c[2],a=cu(c[1]),b=cu(d);switch(e){case
0:var
f=[0,[0,an(b,a3(a,om)),3],0];return[0,[0,an(a,a3(b,on)),3],f];case
1:return[0,[0,an(a,b),0],0];case
2:return[0,[0,an(a,a3(b,oo)),3],0];case
3:return[0,[0,an(b,a3(a,op)),3],0];case
4:return[0,[0,an(a,b),3],0];default:return[0,[0,an(b,a),3],0]}}function
fU(b,a){var
c=iw(b);return ba(function(b){return[0,[0,b,a],0]},c)}function
ix(c){var
d=c[3],e=c[2],a=cu(c[1]),b=cu(d);switch(e){case
0:return[0,[0,an(a,b),0],0];case
1:var
f=[0,[0,an(b,a3(a,oq)),3],0];return[0,[0,an(a,a3(b,or)),3],f];case
2:return[0,[0,an(b,a),3],0];case
3:return[0,[0,an(a,b),3],0];case
4:return[0,[0,an(b,a3(a,os)),3],0];default:return[0,[0,an(a,a3(b,ot)),3],0]}}function
fV(b,a){var
c=ix(b);return ba(function(b){return[0,[0,b,a],0]},c)}var
ou=n[12],ov=0;function
d2(a){return c$(ov,aL,ou,a)}var
ow=n[5],ox=0;function
fW(a,b){return c_(ox,ow,aL,a,b)}function
db(a){return bs(d2,fW,fU,fV,1,a)}function
iy(e,d){var
b=a(n[21],e,d),c=b[1];return typeof
b[2]==="number"?c:a(n[5],c,oy)}function
fX(c,b){var
d=a(n[23],c,b);return a(n[15],d,oz)}function
dc(d){var
a=d;for(;;)switch(a[0]){case
0:return[0,0,a[1]];case
1:var
a=a[2];continue;default:var
e=a[3],b=dc(a[1]),f=b[2],g=b[1],c=dc(e),h=c[2],i=c[1];return[0,fX(fX(g,f),i),h]}}function
dd(b,c){switch(b[0]){case
0:return[0,a(n[22],b[1],c)];case
1:var
d=b[1];return[1,d,dd(b[2],c)];default:var
e=b[2],f=b[1],g=dd(b[3],c);return[2,dd(f,c),e,g]}}function
d3(c){var
e=dc(c),f=e[2],d=e[1];if(a(n[14],d,0)){var
g=iy(b(n[6],f),d),h=b(n[6],g);return[0,dd(cq(n[7],c,f),d),h]}return[0,c,0]}function
d4(d){var
e=d[2],b=d[1];switch(e){case
0:var
f=dc(b),g=f[2],c=f[1];if(a(n[14],c,0))if(bo(aL(g,0)))if(bo(aL(a(n[23],c,g),c)))return 0;return[0,[0,d3(b),0]];case
1:return[0,[0,[0,b,0],e]];case
2:return[0,[0,d3(cq(n[7],b,oA)),3]];default:return[0,[0,d3(b),3]]}}function
iz(a){var
b=a[1],c=a[2];return[0,a3(b[1],[0,b[2]]),c]}function
iA(a){return 0===a[0]?typeof
a[1]==="number"?1:0:0}var
oB=n[12],oC=n[8],oD=n[5],oF=0;function
de(a,b){return bQ(oF,oE,oD,oC,aL,oB,a,b)}function
fY(a){return 0===a?1:3<=a?1:0}var
iB=0;function
cv(a,b){if(b){var
c=b[3],e=b[2],d=b[1];return typeof
a==="number"?[0,d,1,c]:0===a[0]?[0,d,e,cv(a[1],c)]:[0,cv(a[1],d),e,c]}return typeof
a==="number"?oG:0===a[0]?[0,0,0,cv(a[1],0)]:[0,cv(a[1],0),0,0]}function
oH(a){return cv(a,iB)}function
fZ(b,a){if(b){var
c=b[3],d=b[2],e=b[1];if(a){var
f=a[2],g=a[1],h=fZ(c,a[3]),i=d?1:f;return[0,fZ(e,g),i,h]}return b}return a}function
iC(d,c){var
a=d,b=c;for(;;)if(typeof
a==="number")return b;else{if(0===a[0]){var
a=a[1],b=[0,b];continue}var
a=a[1],b=[1,b];continue}}function
iD(a){return iC(a,0)}function
d5(e,j,i,h){var
c=j,d=i,b=h;for(;;){if(c){var
f=c[3],g=c[1];if(c[2]){var
k=d5(e,g,d,[1,b]),c=f,d=a(e,iD(b),k),b=[0,b];continue}var
c=f,d=d5(e,g,d,[1,b]),b=[0,b];continue}return d}}var
aU=[0,iB,cv,oH,fZ,iC,iD,d5,function(c,b,a){return d5(c,b,a,0)}];function
bu(d){var
c=d;for(;;)switch(c[0]){case
0:return aU[1];case
1:return b(aU[3],c[1]);case
2:var
e=c[2],f=bu(c[1]),g=bu(e);return a(aU[4],f,g);case
3:var
h=c[2],i=bu(c[1]),j=bu(h);return a(aU[4],i,j);case
4:var
k=c[2],l=bu(c[1]),m=bu(k);return a(aU[4],l,m);case
5:var
c=c[1];continue;default:var
c=c[1];continue}}function
iE(b){var
c=b[3],d=bu(b[1]),e=bu(c);return a(aU[4],d,e)}function
bS(c){var
b=c;for(;;){if(typeof
b!=="number")switch(b[0]){case
1:return iE(b[1]);case
2:var
d=b[2],e=bS(b[1]),f=bS(d);return a(aU[4],e,f);case
3:var
g=b[2],h=bS(b[1]),i=bS(g);return a(aU[4],h,i);case
4:var
b=b[1];continue;case
5:var
j=b[3],k=bS(b[1]),l=bS(j);return a(aU[4],k,l)}return aU[1]}}function
df(a){return[0,[1,a],3,oI]}function
f0(c,b,a){return[0,[1,c],0,[3,[1,b],[1,a]]]}function
iF(d,c,b){var
e=0;function
f(b,h){var
e=[1,a(u[2],c,b)],f=[0,a(u[2],c,b)],i=g(d,c,b,oJ),j=[1,df(f),i],k=g(d,c,b,oK),l=[2,[1,df(e),k],j],m=g(d,c,b,0);return[2,[2,[1,f0(b,e,f),m],l],h]}return g(aU[8],f,b,e)}function
iG(c,b,a){return[5,iF(c,b,bS(a)),0,a]}function
f1(w,v){var
d=w,c=v;for(;;)if(typeof
c==="number")return 0;else
switch(c[0]){case
0:var
x=c[2],g=de(d,c[1]);if(g){var
h=g[1];if(d2(h))return 1;var
d=[0,h,d],c=x;continue}return 0;case
1:var
y=c[2],i=de(d,c[1]);if(i){var
j=d4(i[1]);if(j){var
d=[0,iz(j[1]),d],c=y;continue}return 1}return 0;default:var
z=c[3],A=c[2],k=de(d,c[1]);if(k){var
B=k[1],l=de(d,A);if(l){var
C=l[1],m=d4(B);if(m){var
o=m[1],p=o[1],q=p[1],D=o[2],E=p[2],r=d4(C);if(r){var
s=r[1],t=s[1],F=s[2],G=t[2],H=t[1];if(fY(D))if(fY(F))if(iA(a3(q,H))){var
f=z,e=b(n[6],E);for(;;){if(f){var
I=f[2],J=f[1],u=f1([0,[0,an(q,[0,e]),0],d],J);if(u){var
f=I,e=a(n[5],e,oL);continue}return u}return a(n[14],e,G)}}return 0}return 1}return 1}return 0}return 0}}function
oM(b,a){return dX(d2,fW,fU,fV,function(a){var
b=ba(bM,a);return function(a){return f1(b,a)}},b,a)}function
f2(a,b){return dY(oO,oN,aM,aT,at,da,a,b)}function
f3(b,a){return fP(oQ,oP,aM,aT,bR,bt,at,b,a)}function
f4(b,a){return fQ(oS,oR,aM,aT,bR,bt,at,b,a)}function
f5(a){return c$(oT,at,da,a)}function
f6(a,b){return c_(oU,aM,at,a,b)}function
f7(a){return Y(oW,oV,aM,aT,bR,bt,at,a)}function
cw(a){return bs(f5,f6,f3,f4,1,a)}function
oX(b,a){return dX(f5,f6,f3,f4,function(a){var
b=ba(bM,a);return function(a){return f2(b,a)}},b,a)}function
iH(a){return 0===a[0]?a[1]:b(n[18],a[1])}function
aD(a){if(typeof
a==="number")return 0===a?oY:oZ;else
switch(a[0]){case
0:return a[1];case
1:return[0,a[1],0];case
2:var
b=a[1],c=aD(a[2]);return aM(aD(b),c);case
3:var
d=a[1],e=aD(a[2]);return bR(aD(d),e);case
4:var
f=a[1],g=aD(a[2]);return aT(aD(f),g);case
5:var
h=a[1],i=iH(a[2]);return fT(aD(h),i);case
6:return fR(aD(a[1]));default:return bt(aD(a[1]))}}function
iI(a,b){return dY(o1,o0,aM,aT,at,da,a,b)}function
iJ(b,a){return fP(o3,o2,aM,aT,bR,bt,at,b,a)}function
iK(b,a){return fQ(o5,o4,aM,aT,bR,bt,at,b,a)}function
iL(a){return c$(o6,at,da,a)}function
iM(a,b){return c_(o7,aM,at,a,b)}aA(981,[0,bo,bM,nf,P,h0,fD,h1,ba,dQ,ng,u,h6,dS,n,aL,nQ,nR,fJ,ae,ia,U,ib,ic,as,bP,cq,c1,c2,fK,fL,ab,I,c3,c4,bq,am,c5,id,c6,ie,Y,br,c7,ig,fM,b4,a1,b5,b6,dV,ih,ii,c8,P,aC,dW,ij,ik,c9,bs,il,dX,fN,fO,im,io,cr,cs,ip,iq,c_,bQ,c$,dY,Y,I,ab,ir,fP,is,fQ,dZ,it,a2,d0,iu,n1,n2,n3,at,da,aM,aT,bt,bR,fR,fS,fT,n6,ct,d1,bb,iv,an,a3,cu,iw,fU,ix,fV,d2,fW,db,iy,fX,dc,dd,d3,d4,iz,iA,de,fY,aU,bu,iE,bS,df,f0,iF,iG,f1,oM,f2,f3,f4,f5,f6,f7,cw,oX,iH,aD,iI,iJ,iK,iL,iM,function(b,a){var
c=a1(function(a){return d0(aD,a)},b);return dX(iL,iM,iJ,iK,function(a){var
b=ba(bM,a);return function(a){return iI(b,a)}},c,a)}],"Micromega_plugin__Micromega");var
o8=a$,D=[0,o8,function(b,a){return b===a?1:0}],v=b(f8[1],[0,D[1]]),iN=v[13],o9=v[1],o_=v[2],o$=v[3],pa=v[4],pb=v[5],pc=v[6],pd=v[7],pe=v[8],pf=v[9],pg=v[10],ph=v[11],pi=v[12],pj=v[14],pk=v[15],pl=v[16],pm=v[17],pn=v[18],po=v[19],pp=v[20],pq=v[21],pr=v[22],ps=v[23],pt=v[24],pu=v[25],pv=v[26],pw=v[27],px=v[28],py=v[29],pz=v[30],pA=v[31],pB=v[32],pC=v[33],pD=v[34],pE=v[35],pF=v[36],pG=v[37],pH=v[38],pI=v[39],s=[0,o9,o_,o$,pa,pb,pc,pd,pe,pf,pg,ph,pi,iN,pj,pk,pl,pm,pn,po,pp,pq,pr,ps,pt,pu,pv,pw,px,py,pz,pA,pB,pC,pD,pE,pF,pG,pH,pI,function(c,b){return a(iN,function(a){return g(k[1],c,pJ,a)},b)}],w=b(b7[1],[0,D[1]]),iO=w[26],pK=w[1],pL=w[2],pM=w[3],pN=w[4],pO=w[5],pP=w[6],pQ=w[7],pR=w[8],pS=w[9],pT=w[10],pU=w[11],pV=w[12],pW=w[13],pX=w[14],pY=w[15],pZ=w[16],p0=w[17],p1=w[18],p2=w[19],p3=w[20],p4=w[21],p5=w[22],p6=w[23],p7=w[24],p8=w[25],p9=w[27],p_=w[28],p$=w[29],qa=w[30],qb=w[31],qc=w[32],qd=w[33],qe=w[34],qf=w[35],qg=w[36],qh=w[37],qi=w[38],r=[0,pK,pL,pM,pN,pO,pP,pQ,pR,pS,pT,pU,pV,pW,pX,pY,pZ,p0,p1,p2,p3,p4,p5,p6,p7,p8,iO,p9,p_,p$,qa,qb,qc,qd,qe,qf,qg,qh,qi,function(c,b){return a(iO,c-1|0,b)[3]}];function
iP(i,d,c,h){var
b=h;for(;;){if(b){var
f=b[2],g=b[1];if(f){a(d,c,g);a(e[55],c,i);var
b=f;continue}return a(d,c,g)}return 0}}function
qj(a,c){try{var
d=b(a,0);b(c,0);return d}catch(a){a=o(a);try{b(c,0)}catch(b){throw a}throw a}}function
qk(e,d){var
a=e;for(;;){if(a){var
f=a[2],c=b(a[1][1],d);if(c)return c;var
a=f;continue}return 0}}function
iQ(e,d){var
c=0,b=d;for(;;){if(b){var
i=b[2],j=b[1],h=function(d){return function(c,b){return[0,a(e,d,b),c]}}(j),c=g(f[20],h,c,b),b=i;continue}return c}}function
iR(g,f,e){var
c=f,b=e;for(;;){if(c){var
h=c[2],i=c[1];if(b){var
d=b[2];if(a(g,i,b[1])){var
c=h,b=d;continue}var
b=d;continue}return 0}return 1}}function
bv(h,a){function
c(e,a){var
c=e[2],d=e[1];if(d)return[0,d,[0,a,c]];var
f=b(h,a);return f?[0,[0,[0,f[1],a]],c]:[0,d,[0,a,c]]}return g(f[20],c,ql,a)}function
qm(j,p,i){var
m=bv(j,i),n=m[1];if(n){var
o=n[1],g=o[1],f=o[2],c=0,d=m[2];for(;;){if(d){var
h=d[2],e=d[1],k=b(j,e);if(k){var
l=k[1];if(a(p,l,g)){var
g=l,q=[0,f,c],f=e,c=q,d=h;continue}var
c=[0,e,c],d=h;continue}var
c=[0,e,c],d=h;continue}return[0,[0,[0,g,f]],c]}}return[0,0,i]}function
iS(e,d){var
a=d;for(;;){if(a){var
f=a[2],c=b(e,a[1]);if(c)return[0,c[1]];var
a=f;continue}return 0}}function
iT(h,a){function
c(c,a){var
d=c[2],e=c[1],f=b(h,a);return f?[0,[0,[0,f[1],a],e],d]:[0,e,[0,a,d]]}return g(f[20],c,qn,a)}function
d6(h,c){function
d(c,a){var
d=c[1],f=c[2],e=b(h,a);return e?[0,[0,e[1],d],1]:[0,[0,a,d],f]}var
a=g(f[20],d,qo,c),e=a[1];return a[2]?[0,e]:0}function
iU(e,c){var
d=0;function
a(a,d){var
c=b(e,d);return c?[0,c[1],a]:a}return g(f[20],a,d,c)}function
iV(j,i,c){function
d(l,k){var
c=l,d=k;for(;;){var
f=bv(j,d),g=f[1];if(g){var
h=f[2],m=g[1],n=a(e[26],h,c),o=iU(b(i,m),n),c=a(e[26],o,c),d=h;continue}return c}}try{var
f=d(0,c);return f}catch(a){a=o(a);b(d7[4],e[28]);throw a}}function
iW(d,c){var
b=a(l[17],d,c),e=a(l[15],d,b),f=a(l[15],c,b),g=a(l[10],e,f);return a(l[10],b,g)}function
a4(a){return 2===a[0]?b(dg[3],a[1]):l[2]}function
af(a){switch(a[0]){case
0:return b(l[36],a[1]);case
1:return a[1];default:return b(dg[2],a[1])}}function
d8(e,d){var
a=d;for(;;){var
c=b(e,a);if(c){var
a=c[1];continue}return a}}function
iX(e,d){var
a=e;for(;;){if(a){var
f=a[2],c=b(a[1],d);if(c)return[0,c[1]];var
a=f;continue}return 0}}function
iY(a){return a?iY(a[1])+1|0:0}function
d9(a){return typeof
a==="number"?1:0===a[0]?1+(2*d9(a[1])|0)|0:2*d9(a[1])|0}function
qp(a){return a?d9(a[1]):0}function
f9(a){return typeof
a==="number"?1:0===a[0]?1+(2*f9(a[1])|0)|0:2*f9(a[1])|0}function
d_(b){if(typeof
b==="number")return l[2];else{if(0===b[0]){var
c=d_(b[1]),d=a(l[11],2,c);return a(l[7],1,d)}var
e=d_(b[1]);return a(l[11],2,e)}}function
f_(a){if(typeof
a==="number")return l[1];else{if(0===a[0])return d_(a[1]);var
c=d_(a[1]);return b(l[3],c)}}function
qq(b){var
c=b[1],e=[1,f_([0,b[2]])],f=[1,f_(c)];return a(d[9],f,e)}function
iZ(a){return 0===a?0:[0,iZ(a-1|0)]}function
cx(b){return a(D[2],b,1)?0:a(D[2],b&1,1)?[0,cx(b>>>1|0)]:[1,cx(b>>>1|0)]}function
qr(b){if(0<=b)return a(D[2],b,0)?0:[0,cx(b)];throw[0,aV,qs]}function
f$(b){return a(D[2],b,1)?0:a(D[2],b&1,1)?[0,f$(b>>>1|0)]:[1,f$(b>>>1|0)]}function
qt(a){var
b=a$(a,0);return 0===b?0:1===b?[0,cx(a)]:[1,cx(-a|0)]}function
d$(d){var
f=b(l[36],2);function
c(b){if(a(l[24],b,l[2]))return 0;var
d=a(l[14],b,f),e=d[1];return a(l[24],l[2],d[2])?[0,c(e)]:[1,c(e)]}return c(d)}function
i0(a){var
c=b(l[22],a);return 0===c?0:1===c?[0,d$(a)]:[1,d$(b(l[3],a))]}function
qu(a){var
b=d$(a4(a));return[0,i0(af(a)),b]}function
qv(e){var
c=e;for(;;){if(c){var
f=c[2],d=b(c[1],0);if(a(D[2],d,0)){var
c=f;continue}return d}return 0}}function
qw(g,f,e){var
c=f,b=e;for(;;){if(c){if(b){var
h=b[2],i=c[2],d=a(g,c[1],b[1]);if(a(D[2],d,0)){var
c=i,b=h;continue}return d}return 1}return b?-1:0}}function
qx(a){return a}function
qy(a){return a+1|0}var
qz=e[1][5];function
qA(d,c){var
f=b(e[22],c);return a(e[55],d,f)}var
qB=D[1];function
qC(a){return a}var
R=b(f8[1],[0,qB]);function
qD(c){for(;;)try{var
d=a(O[15],0,c)[2];return d}catch(a){a=o(a);if(a[1]===O[1]){var
b=a[2];if(typeof
b==="number")if(11===b)continue}throw a}}function
i1(c,t,s){var
h=a(O[67],0,0),i=h[2],j=h[1],l=a(O[67],0,0),m=l[2],n=l[1],p=a(O[67],0,0),q=p[2],u=p[1],v=T(O[69],c,t,j,m,q),r=b(O[31],i);a(e[61],r,s);b(e[52],r);var
d=qD(v);function
w(e){var
c=[0,j,[0,i,[0,n,[0,m,[0,u,[0,q,0]]]]]];function
d(a){try{var
c=b(O[24],a);return c}catch(a){return 0}}return a(f[15],d,c)}return qj(function(q){switch(d[0]){case
0:var
a=d[1];if(0===a){var
f=b(O[30],n);try{var
j=b(ea[3],f);return j}catch(a){a=o(a);var
h=b(d7[1],a),i=g(k[4],qE,c,h);return b(e[3],i)}}var
l=g(k[4],qF,c,a);return b(e[3],l);case
1:var
m=g(k[4],qG,c,d[1]);return b(e[3],m);default:var
p=g(k[4],qH,c,d[1]);return b(e[3],p)}},w)}var
ag=[0,f_,qq,d9,qp,iY,f9],y=[0,cx,i0,qr,iZ,qu,f$,qt,d$],b8=[0,R[1],R[2],R[3],R[4],R[5],R[6],R[7],R[8],R[9],R[10],R[11],R[12],R[13],R[15],R[16],R[17],R[18],R[19],R[20],R[21],R[22],R[24],R[26],R[28]],bw=[0,qA,qy,qz,qx,qC],ga=[0,qw,qv];aA(991,[0,D,s,r,af,a4,ga,bw,b8,iP,y,ag,iW,iQ,qk,iR,bv,iT,qm,iS,d8,d6,iV,iU,iX,i1],"Micromega_plugin__Mutils");function
cy(k,j){var
c=k,b=j;for(;;){if(c){var
e=c[1],l=c[2],m=e[2],n=e[1];if(b){var
f=b[1],o=b[2],p=f[2],g=a(D[2],n,f[1]);if(g){var
h=a(d[26],m,p);if(h){var
c=l,b=o;continue}var
i=h}else
var
i=g;return i}return 0}return b?0:1}}function
i2(f){var
c=0,a=f;for(;;){if(a){var
e=a[1],g=a[2],h=e[1],i=[0,h,b(d[56],e[2])],c=c+b(bc[27],i)|0,a=g;continue}return b(bc[27],c)}}var
K=0;function
bx(a){if(a){var
b=a[1];if(0===b[1])var
c=b[2],d=0===c[0]?0===c[1]?a[2]?0:1:0:0;else
var
d=0;if(!d)return 0}return 1}function
i3(h,e,i){var
c=i[2],f=i[1];if(a(D[2],f,0)){if(a(d[32],qI,c))return 0;var
l=b(d[40],c);return g(k[1],e,qJ,l)}if(0===c[0]){var
j=c[1]+1|0;if(!(2<j>>>0))switch(j){case
0:return C(k[1],e,qL,h,f);case
1:return 0;default:return a(h,e,f)}}var
m=b(d[40],c);return T(k[1],e,qK,m,h,f)}function
eb(d,c,b){if(b){var
f=b[2],g=b[1];if(f){var
h=function(a,b){return eb(d,a,b)},i=function(a,b){return i3(d,a,b)};return W(k[1],c,qQ,i,g,h,f)}return i3(d,c,g)}return a(e[55],c,qR)}function
ec(b,a){return g(k[1],b,qS,a)}function
b9(b,a){return eb(ec,b,a)}function
qT(e,c){function
h(e,c){function
h(c){function
f(f,i){var
c=i[2],e=i[1];if(a(D[2],e,0)){if(a(d[32],qM,c))return 0;var
j=b(d[40],c);return g(k[1],f,qN,j)}if(0===c[0]){var
h=c[1]+1|0;if(!(2<h>>>0))switch(h){case
0:return C(k[1],f,qP,ec,e);case
1:return 0;default:return ec(f,e)}}var
l=b(d[40],c);return T(k[1],f,qO,l,ec,e)}return C(k[1],e,qU,f,c)}return a(f[15],h,c)}return C(k[1],e,qV,h,c)}function
b_(b){function
e(i,h){var
c=i,b=h;for(;;){if(b){var
f=b[2],g=b[1];if(a(d[31],g,qW))return[0,[0,c,g],e(c+1|0,f)];var
c=c+1|0,b=f;continue}return 0}}return e(0,b)}function
gc(a){function
b(c,a){if(a){var
d=a[1],e=a[2],f=d[2];return c===d[1]?[0,f,b(c+1|0,e)]:[0,gb,b(c+1|0,a)]}return 0}return b(0,a)}function
cz(e,c,b){return a(d[26],c,qX)?b:[0,[0,e,c],b]}function
gd(f,d,c){if(c){var
h=c[2],i=c[1],j=i[2],g=i[1],k=a(D[1],f,g)+1|0;if(2<k>>>0)return b(e[3],qY);switch(k){case
0:return cz(f,b(d,gb),c);case
1:return cz(g,b(d,j),h);default:return[0,[0,g,j],gd(f,d,h)]}}return cz(f,b(d,gb),0)}function
F(f,d,c){if(c){var
h=c[2],i=c[1],g=i[1],k=i[2],j=a(D[1],f,g)+1|0;if(2<j>>>0)return b(e[3],qZ);switch(j){case
0:return cz(f,d,c);case
1:return cz(g,d,h);default:return[0,[0,g,k],F(f,d,h)]}}return cz(f,d,0)}function
ge(b){return a(d[26],b,q0)?0:[0,[0,0,b],0]}function
ao(b,c){if(0===b[0]){var
e=b[1];if(0===e)return 0;if(1===e)return c}function
g(c){var
e=c[1];return[0,e,a(d[7],b,c[2])]}return a(f[17],g,c)}function
cA(c,b){if(a(d[31],c,q1)){var
e=function(b){var
e=b[1];return[0,e,a(d[9],b[2],c)]};return a(f[17],e,b)}return b}function
cB(c){function
e(a){var
c=a[1];return[0,c,b(d[3],a[2])]}return a(f[17],e,c)}function
aW(q,p){var
c=q,b=p;for(;;){if(c){if(b){var
e=b[2],h=b[1],i=h[2],j=h[1],f=c[2],k=c[1],l=k[2],g=k[1],m=a$(g,j);if(0===m){var
n=a(d[2],l,i);if(a(d[32],q2,n)){var
c=f,b=e;continue}return[0,[0,g,n],aW(f,e)]}return 0<=m?[0,[0,j,i],aW(e,c)]:[0,[0,g,l],aW(f,b)]}var
o=c}else
var
o=b;return o}}function
ed(c,r,b,q){var
f=r,e=q;for(;;){if(f){if(e){var
g=e[2],j=e[1],k=j[2],l=j[1],h=f[2],m=f[1],n=m[2],i=m[1],o=a$(i,l);if(0===o){var
s=a(d[6],b,k),t=a(d[6],c,n),p=a(d[1],t,s);if(a(d[32],q3,p)){var
f=h,e=g;continue}return[0,[0,i,p],ed(c,h,b,g)]}if(0<=o){var
u=ed(c,f,b,g);return[0,[0,l,a(d[6],b,k)],u]}var
v=ed(c,h,b,e);return[0,[0,i,a(d[6],c,n)],v]}return ao(c,f)}return ao(b,e)}}function
gf(f,e,c,b){if(a(d[26],f,q4))if(a(d[26],c,q5))return aW(e,b);return ed(f,e,c,b)}function
q6(e,c){var
f=0,g=[0,function(b){return a(d[37],e[2],c[2])},f],h=[0,function(b){return a(D[1],e[1],c[1])},g];return b(ga[2],h)}var
gg=b(ga[1],q6);function
au(i,h){var
b=h;for(;;){if(b){var
d=b[1],f=b[2],g=d[2],e=a(D[1],d[1],i);if(-1===e){var
b=f;continue}var
c=0===e?[0,[0,g,b]]:0}else
var
c=0;return c?c[1][1]:q7}}function
i4(a){if(a){var
b=0===a[1][1]?a[2]?0:1:0;if(!b)return 0}return 1}function
dh(a){if(a){var
b=a[1];if(0===b[1])return b[2]}return q8}function
aX(a){if(a){var
b=a[1];return[0,[0,b[1],b[2],a[2]]]}return 0}function
i5(c){var
a=c;for(;;){if(a){var
b=a[2],d=a[1][1];if(b){var
a=b;continue}return d+1|0}return 1}}function
gh(b){var
c=s[1];function
d(c,b){return a(s[4],b[1],c)}return g(f[20],d,c,b)}function
bd(a){if(a){var
b=a[1];if(0===b[1])return[0,b[2],a[2]]}return[0,q9,a]}function
q_(b,f){var
a=f;for(;;){if(a){var
c=a[2],d=a[1],e=d[1],g=d[2];if(X(b,e))return[0,g,c];if(bZ(b,e))return[0,q$,a];var
a=c;continue}return[0,ra,K]}}function
i6(a){return a?[0,a[1],a[2]]:rb}function
_(c,b,a){function
d(b,a){return g(c,b,a[1],a[2])}return g(f[20],d,b,a)}function
rc(h,f,e){var
b=f,a=e;for(;;){if(a){var
c=a[1],i=a[2],d=g(h,b,c[1],c[2]);if(d){var
b=d[1],a=i;continue}return 0}return[0,b]}}function
gi(f,e){var
b=e;for(;;){if(b){var
c=b[1],g=b[2],d=a(f,c[1],c[2]);if(d)return[0,d[1]];var
b=g;continue}return 0}}function
ee(c,b){function
d(b){return a(c,b[1],b[2])}return a(f[27],d,b)}function
i7(c,b){function
d(a){return[0,a[1]-c|0,a[2]]}return a(f[17],d,b)}function
rd(c,b){function
d(a){return[0,a[1]+c|0,a[2]]}return a(f[17],d,b)}function
gj(c){var
d=l[1],b=_(function(c,h,b){var
d=l[2],e=a4(b),f=a(l[23],e,d);if(a(D[2],f,0)){var
g=af(b);return a(l[17],c,g)}throw[0,aV,re]},d,c),e=a(l[23],b,l[1]);return a(D[2],e,0)?l[2]:b}function
cC(b){var
e=l[2],g=_(function(b,c,a){return iW(b,a4(a))},e,b),h=l[1],c=_(function(c,e,b){var
d=af(b);return a(l[17],c,d)},h,b),i=a(l[23],c,l[1]),j=a(D[2],i,0)?l[2]:c;function
k(b){var
c=b[1],e=a(d[6],b[2],[1,g]);return[0,c,a(d[9],e,[1,j])]}return a(f[17],k,b)}function
i8(n,m,l){var
c=m,b=l;for(;;){if(c)if(b){var
e=b[2],f=b[1],g=f[2],h=f[1],i=c[2],j=c[1],k=j[2],d=j[1];if(a(D[2],d,h)){if(a(n,k,g))return[0,[0,d,k,g]];var
c=i,b=e;continue}if(d<h){var
c=i;continue}var
b=e;continue}return 0}}function
ef(m,l){var
e=rf,c=m,b=l;for(;;){if(c)if(b){var
f=b[2],g=b[1],h=g[1],i=c[2],j=c[1],k=j[1],n=g[2],o=j[2];if(k===h){var
p=a(d[6],o,n),e=a(d[1],e,p),c=i,b=f;continue}if(bZ(k,h)){var
c=i;continue}var
b=f;continue}return e}}function
rg(c,b){function
d(b){return a(c,b[1],b[2])}return a(f[17],d,b)}function
rh(c){if(c){var
e=c[1],h=c[2],i=[0,e[1],e[2]],j=function(e,c){var
f=c[2],g=e[2],h=c[1],i=e[1],j=b(d[15],f),k=b(d[15],g);return a(d[27],k,j)?[0,i,g]:[0,h,f]};return[0,g(f[20],j,i,h)]}return 0}function
i9(c){function
d(b){return a(c,b[1],b[2])}return b(f[37],d)}aA(993,[0,i2,cy,gg,eb,b9,qT,gh,dh,bd,q_,i6,ge,i4,K,bx,au,F,function(a){return F(a,ri,K)},gd,i5,aX,b_,gc,i7,rd,gj,cC,aW,ao,gf,cA,cB,_,rc,gi,ee,i8,ef,rg,rh,i9],"Micromega_plugin__Vect");var
gk=[0,e[8]],rk=d[2],rl=d[7],Z=b(b7[1],[0,D[1]]),rj=0;function
i_(d){try{var
e=b(Z[24],d),f=e[1],i=e[2],g=a(Z[26],f,d),j=g[3];if(b(Z[2],g[1]))if(b(Z[2],j))var
h=[0,[0,f,i]],c=1;else
var
c=0;else
var
c=0;if(!c)var
h=0;return h}catch(a){a=o(a);if(a===L)return 0;throw a}}function
eg(e,a){function
c(b,a){var
c=a[2],d=a[1];return 1===c?g(k[1],b,rm,d):C(k[1],b,rn,d,c)}function
d(b,a){if(a){var
e=a[2],f=a[1];return e?W(k[1],b,ro,c,f,d,e):c(b,f)}return 0}return d(e,b(Z[19],a))}var
di=Z[1];function
i$(a){var
b=0;function
c(c,b,a){return a+b|0}return g(Z[13],c,a,b)}function
eh(c,b){var
d=i$(c),e=i$(b);return a(D[2],d,e)?g(Z[10],D[1],c,b):a(D[1],d,e)}function
ei(a){return X(a,Z[1])}function
ej(a){return g(Z[4],a,1,Z[1])}function
ja(b){var
a=i_(b);return a?1===a[1][2]?1:0:0}function
gl(c){var
a=i_(c);if(a){var
b=a[1],d=b[1];return 1===b[2]?[0,d]:0}return 0}function
jb(a){if(ei(a))return 0;try{var
b=function(c,a,b){var
d=a/2|0;if(0===(a%2|0))return g(Z[4],c,d,b);throw L},c=[0,g(Z[13],b,a,di)];return c}catch(a){a=o(a);if(a===L)return 0;throw a}}function
gm(c,b){try{var
d=a(Z[27],c,b);return d}catch(a){a=o(a);if(a===L)return 0;throw a}}function
gn(b,a){function
c(b,c,a){var
d=gm(b,a)+c|0;return g(Z[4],b,d,a)}return g(Z[13],c,b,a)}function
jc(c,b){var
f=e[8];function
h(f,d,b){var
g=B.caml_div(gm(f,c),d);return a(e[5],b,g)}var
d=g(Z[13],h,b,f),i=Z[1];function
j(c,f,a){var
e=f-ft(gm(c,b),d)|0;return 0===e?a:g(Z[4],c,e,a)}return[0,g(Z[13],j,c,i),d]}function
jd(b){var
c=s[1];function
d(c,d,b){return a(s[4],c,b)}return g(Z[13],d,b,c)}var
je=Z[13],z=b(b7[1],[0,eh]),jf=z[8],rp=z[1],rq=z[2],rr=z[3],rs=z[4],rt=z[5],ru=z[6],rv=z[7],rw=z[10],rx=z[11],ry=z[12],rz=z[13],rA=z[14],rB=z[15],rC=z[16],rD=z[17],rE=z[18],rF=z[19],rG=z[20],rH=z[21],rI=z[22],rJ=z[23],rK=z[24],rL=z[25],rM=z[26],rN=z[27],rO=z[28],rP=z[29],rQ=z[30],rR=z[31],rS=z[32],rT=z[33],rU=z[34],rV=z[35],rW=z[36],rX=z[37],rY=z[38],cD=[0,rp,rq,rr,rs,rt,ru,rv,jf,rw,rx,ry,rz,rA,rB,rC,rD,rE,rF,rG,rH,rI,rJ,rK,rL,rM,rN,rO,rP,rQ,rR,rS,rT,rU,rV,rW,rX,rY,function(e){return b(jf,function(f,b,a){if(b){var
c=b[1];if(a)return g(e,f,c,a[1]);var
d=c}else{if(!a)return 0;var
d=a[1]}return[0,d]})}];function
rZ(e,h){var
c=h[2],f=h[1];if(ei(f)){if(a(d[32],r0,c))return 0;var
j=b(d[40],c);return g(k[1],e,r1,j)}if(0===c[0]){var
i=c[1]+1|0;if(!(2<i>>>0))switch(i){case
0:return C(k[1],e,r3,eg,f);case
1:return 0;default:return eg(e,f)}}var
l=b(d[40],c);return T(k[1],e,r2,l,eg,f)}var
ah=b(b7[1],[0,eh]);function
jg(c,b){function
d(b,a){return C(k[1],c,r4,rZ,[0,b,a])}return a(ah[12],d,b)}function
jh(c,b){try{var
d=a(ah[27],c,b);return d}catch(a){a=o(a);if(a===L)return r5;throw a}}function
r6(a){var
b=ah[1],c=ej(a);return g(ah[4],c,r7,b)}function
dj(a){return g(ah[4],di,a,ah[1])}function
dk(e,f,c){if(0===b(d[25],f))return c;var
h=a(rk,jh(e,c),f);return 0===b(d[25],h)?a(ah[7],e,c):g(ah[4],e,h,c)}function
ji(b,a){function
c(c,b,a){return dk(c,b,a)}return g(ah[13],c,b,a)}function
jj(c,i){var
e=ah[1];function
f(k,c,j){if(0===b(d[25],c))var
e=dj(r8);else
var
f=ah[1],h=function(e,d,b){var
f=a(rl,c,d),h=gn(k,e);return g(ah[4],h,f,b)},e=g(ah[13],h,i,f);return ji(e,j)}return g(ah[13],f,c,e)}function
r9(c){function
e(a){return b(d[3],a)}return a(ah[33],e,c)}var
jk=ah[13],ek=[aS,sa,aR(0)];function
jl(a){return 2===a[2]?1:0}function
el(a){switch(a){case
0:return d[26];case
1:return d[30];default:return d[28]}}function
dl(a){switch(a){case
0:return sb;case
1:return sc;default:return sd}}function
se(c,a){var
e=a[2],f=a[1],g=b(d[40],a[3]),h=dl(e);return W(k[1],c,sf,b9,f,h,g)}function
go(c,b){switch(c){case
2:if(2<=b)return 2;var
a=0;break;case
1:var
a=0;break;default:var
a=1}if(!a)if(0!==b)return 1;return 0}function
em(c,a){switch(c){case
0:var
d=a,b=1;break;case
1:if(1===a)return 1;var
b=0;break;default:var
b=0}if(!b){if(0!==a)return 2;var
d=c}return d}var
en=b(b7[1],[0,eh]),eo=b(b7[1],[0,D[1]]),ep=[0,en[1]],eq=[0,eo[1]],gp=[0,0];function
sg(a){ep[1]=en[1];eq[1]=eo[1];gp[1]=0;return 0}function
dm(b){try{var
d=a(en[27],b,ep[1]);return d}catch(a){a=o(a);if(a===L){var
c=gp[1];ep[1]=g(en[4],b,c,ep[1]);eq[1]=g(eo[4],c,b,eq[1]);gp[1]++;return c}throw a}}function
by(b){return a(eo[27],b,eq[1])}dm(di);function
jm(a){return F(dm(ej(a)),sh,K)}function
si(a){return F(dm(a),sj,K)}function
er(a){return g(jk,function(c,b,a){return F(dm(c),b,a)},a,K)}function
dn(a){var
b=dj(sk);return _(function(c,b,a){return dk(by(b),a,c)},b,a)}function
gq(a,c){var
d=[0,b(a,sm)];return _(function(h,f,e){var
i=by(f),c=[0,b(a,sl)],d=g(je,function(d,c,a){var
e=b(y[3],c);return[4,[6,[1,b(y[1],d)],e],a]},i,c);return[2,[4,[0,b(a,e)],d],h]},d,c)}function
jn(c,b){try{var
a=eg(c,by(b));return a}catch(a){a=o(a);if(a===L)return g(k[1],c,sn,b);throw a}}function
es(b,a){return eb(jn,b,a)}function
b$(a){return 0===b(d[25],a)?K:F(0,a,K)}function
so(a){return ee(function(c,d){var
a=by(c),b=ja(a);return b?b:ei(a)},a)}function
sp(e){var
b=i6(e),c=b[1],f=c[2],g=c[1];if(bx(b[2]))if(a(d[28],f,sq))return gl(by(g));return 0}function
et(h,f){var
i=dn(f),c=ej(h),b=dj(r_),d=[0,dj(r$),b];function
e(f,e,d){var
g=d[2],h=d[1],i=jc(f,c),j=i[2],k=i[1];if(0===j)return[0,h,dk(f,e,g)];var
b=di,a=j-1|0;for(;;){if(0===a)return[0,dk(gn(k,b),e,h),g];var
b=gn(b,c),a=a-1|0;continue}}var
a=g(ah[13],e,i,d),j=a[1],k=er(a[2]);return[0,er(j),k]}function
jo(b,a){return i4(et(b,a)[1])}function
jp(f,c){var
a=0;return _(function(a,h,g){if(b(f,g)){var
d=gl(by(h));if(d){var
e=d[1];return jo(e,c)?[0,e,a]:a}return a}return a},a,c)}function
jq(c,b){var
a=jp(c,b);return a?[0,g(f[20],e[1][4],a[1],a[2])]:0}function
bz(b,a){var
c=dn(a);return er(jj(dn(b),c))}function
gr(b,a){return aW(b,a)}function
sr(a){return _(function(c,b,a){var
d=b$(a);return gr(bz(jm(b),d),c)},K,a)}function
gs(b){var
c=s[1];return _(function(c,b,e){var
d=jd(by(b));return a(s[7],d,c)},c,b)}function
ss(e,b,c){var
h=s[1];function
i(c,b){var
d=gs(b[1]);return a(s[7],c,d)}var
d=g(f[20],i,h,c);function
j(b,f){function
c(a){return C(k[1],b,st,a,e)}return a(s[13],c,d)}C(k[1],b,su,j,d);function
l(c,a){var
d=a[1],e=dl(a[2]);return W(k[1],b,sv,c,es,d,e)}a(f[16],l,c);return a(k[1],b,sw)}function
sx(a){var
b=cD[1];return _(function(a,d,e){var
b=by(d),c=jb(b);return c?g(cD[4],c[1],b,a):a},b,a)}function
aE(e,c){if(typeof
c==="number")return a(k[1],e,sy);else
switch(c[0]){case
0:return T(k[1],e,sz,aE,c[2],c[1]);case
1:return g(k[1],e,sA,c[1]);case
2:return g(k[1],e,sB,c[1]);case
3:var
f=b(d[40],c[1]);return g(k[1],e,sC,f);case
4:var
h=dn(c[1]);return C(k[1],e,sD,jg,h);case
5:var
i=c[2],j=dn(c[1]);return W(k[1],e,sE,jg,j,aE,i);case
6:var
m=c[2],n=b(l[33],c[1]);return T(k[1],e,sF,aE,m,n);case
7:return W(k[1],e,sG,aE,c[1],aE,c[2]);case
8:return W(k[1],e,sH,aE,c[1],aE,c[2]);default:return C(k[1],e,sI,aE,c[1])}}function
gt(c,b){if(typeof
b==="number")return a(k[1],c,sJ);else{if(0===b[0])return fu(k[1],c,sK,b[1],aE,b[2],gt,b[3]);var
d=b[5],e=b[4],f=b[3],g=b[2],h=b[1],i=function(a,b){return iP(sL,gt,a,b)};return Iy(k[1],c,sM,h,aE,g,b9,f,aE,e,i,d)}}function
eu(c){var
b=c;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
b=b[2];continue;case
1:return sO;case
2:return sP;case
3:return b[1];case
5:var
b=b[2];continue;case
6:var
e=[1,b[1]],f=eu(b[2]);return a(d[9],f,e);case
9:var
b=b[1];continue;case
4:break;default:var
g=b[1],h=eu(b[2]),i=eu(g);return a(d[1],i,h)}return sN}}function
cE(f){var
b=f;for(;;){if(typeof
b==="number")var
c=0;else
switch(b[0]){case
0:var
b=b[2];continue;case
9:var
d=b[1],c=1;break;case
1:case
2:return b[1];case
5:case
6:var
d=b[2],c=1;break;case
7:case
8:var
g=b[1],h=cE(b[2]),i=cE(g);return a(e[6],i,h);default:var
c=0}if(c){var
b=d;continue}return-1}}function
ev(b){if(typeof
b==="number")return-1;else{if(0===b[0]){var
c=b[2],d=b[1],h=ev(b[3]),i=cE(c),j=a(e[6],i,h);return a(e[6],d,j)}var
k=b[5],l=b[2],m=b[1],n=cE(b[4]),o=cE(l),p=a(e[6],o,n),q=a(e[6],m,p),r=function(c,b){var
d=ev(b);return a(e[6],c,d)};return g(f[20],r,q,k)}}function
bA(c,n){var
b=n;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
b=b[2];continue;case
5:var
o=b[1],d=bA(c,b[2]);return[0,d[1],d[2],[5,o,d[3]]];case
6:var
f=bA(c,b[2]),g=f[2];return[0,[0,[0,g,f[3]],f[1]],g+1|0,[2,g]];case
7:var
p=b[2],h=bA(c,b[1]),q=h[3],r=h[1],i=bA(h[2],p),s=i[2],t=[7,q,i[3]];return[0,a(e[26],i[1],r),s,t];case
8:var
u=b[2],j=bA(c,b[1]),v=j[3],w=j[1],k=bA(j[2],u),x=k[2],y=[8,v,k[3]];return[0,a(e[26],k[1],w),x,y];case
9:var
l=bA(c,b[1]),m=l[2];return[0,[0,[0,m,l[3]],l[1]],m+1|0,[2,m]]}return[0,0,c,b]}}function
gu(c,a){if(typeof
a!=="number"&&9===a[0]){var
b=bA(c,a[1]);return[0,b[1],b[2],[9,b[3]]]}return bA(c,a)}function
jr(b){var
a=b;for(;;){if(typeof
a!=="number"&&9===a[0]){var
a=a[1];continue}return a}}function
cF(e){var
b=e;for(;;){if(typeof
b==="number")var
c=0;else
switch(b[0]){case
0:var
b=b[2];continue;case
9:var
d=b[1],c=1;break;case
1:case
2:return a(s[4],b[1],s[1]);case
5:case
6:var
d=b[2],c=1;break;case
7:case
8:var
f=b[1],g=cF(b[2]),h=cF(f);return a(s[7],h,g);default:var
c=0}if(c){var
b=d;continue}return s[1]}}function
gv(h,o){var
c=o;for(;;)if(typeof
c==="number")return[0,h,0];else{if(0===c[0]){var
d=c[2],l=c[1];if(typeof
d!=="number"&&6===d[0])if(typeof
c[3]==="number"){var
c=[0,l,d[2],0];continue}var
p=c[3],i=gu(h,d),q=i[3],r=i[1],m=gv(i[2],p),s=m[1],t=[0,l,q,m[2]],u=function(b,a){return[0,a[1],[9,a[2]],b]};return[0,s,g(f[20],u,t,r)]}var
v=c[5],w=c[4],x=c[3],y=c[1],j=gu(h,jr(c[2])),z=j[3],A=j[2],B=j[1],k=gu(A,jr(w)),C=k[3],D=k[2],E=k[1],F=function(a){return gv(D,a)},G=a(f[17],F,v),n=b(f[46],G),H=n[2],I=n[1],J=a(e[26],E,B),K=[1,y,z,x,C,H],L=function(b,a){return[0,a[1],[9,a[2]],b]},M=g(f[20],L,K,J);return[0,g(f[20],e[6],0,I),M]}}function
js(d,c){function
e(c){if(typeof
c==="number")return[0,0,s[1]];else{if(0===c[0]){var
j=c[3],h=c[2],d=c[1];if(typeof
j==="number"){var
q=cF(h);return[0,c,a(s[4],d,q)]}var
k=e(j),i=k[2],l=k[1];if(a(s[3],d,i)){var
r=cF(h),t=a(s[7],r,i);return[0,[0,d,h,l],a(s[4],d,t)]}return[0,l,i]}var
m=c[4],n=c[2],o=c[1],u=c[3],v=a(f[17],e,c[5]),p=b(f[46],v),w=p[1],x=g(f[20],s[7],s[1],p[2]),y=cF(m),z=cF(n),A=a(s[7],z,y),B=a(s[7],A,x);return[0,[1,o,n,u,m,w],a(s[4],o,B)]}}return gv(d,e(c)[1])}function
jt(a){if(typeof
a==="number")return 4;else
switch(a[0]){case
0:return 0;case
1:return 1;case
2:return 2;case
3:return 3;case
4:return 5;case
5:return 6;case
6:return 7;case
7:return 8;case
8:return 9;default:return 10}}function
ew(f,e,c,b){var
g=b[2],h=c[2],d=a(f,c[1],b[1]);return 0===d?a(e,h,g):d}function
ca(h,g){var
c=h,b=g;for(;;){if(typeof
c==="number"){if(typeof
b==="number")return 0}else
switch(c[0]){case
0:if(typeof
b!=="number"&&0===b[0]){var
e=b[1],f=c[1],j=b[2],k=c[2];if(l4(f,e)){var
c=k,b=j;continue}return B.caml_string_compare(f,e)}break;case
1:if(typeof
b!=="number"&&1===b[0])return a$(c[1],b[1]);break;case
2:if(typeof
b!=="number"&&2===b[0])return a$(c[1],b[1]);break;case
3:if(typeof
b!=="number"&&3===b[0])return a(d[37],c[1],b[1]);break;case
4:if(typeof
b!=="number"&&4===b[0])return a(gg,c[1],b[1]);break;case
5:if(typeof
b!=="number"&&5===b[0])return ew(gg,ca,[0,c[1],c[2]],[0,b[1],b[2]]);break;case
6:if(typeof
b!=="number"&&6===b[0])return ew(l[23],ca,[0,c[1],c[2]],[0,b[1],b[2]]);break;case
7:if(typeof
b!=="number"&&7===b[0])return ew(ca,ca,[0,c[1],c[2]],[0,b[1],b[2]]);break;case
8:if(typeof
b!=="number"&&7===b[0])return ew(ca,ca,[0,c[1],c[2]],[0,b[1],b[2]]);break;default:if(typeof
b!=="number"&&9===b[0]){var
c=c[1],b=b[1];continue}}var
i=jt(b);return a$(jt(c),i)}}function
ex(b,a){if(typeof
b==="number")var
c=a;else{if(typeof
a!=="number")return[8,b,a];var
c=b}return c}function
cG(e,c){if(typeof
c!=="number")switch(c[0]){case
0:var
g=c[1];return[0,g,cG(e,c[2])];case
5:var
h=c[2];return[5,ao(e,c[1]),h]}var
f=b(d[25],e)+1|0;if(2<f>>>0)throw[0,aV,sQ];switch(f){case
0:return[5,b$(e),c];case
1:return 0;default:return a(d[32],sR,e)?c:[7,[3,e],c]}}function
ju(b,a){var
c=bd(b),d=c[1];return bx(c[2])?cG(d,a):[5,b,a]}function
jv(b,a){if(typeof
b!=="number")if(typeof
a!=="number"){if(typeof
b==="number")var
c=0;else
if(3===b[0])var
f=a,e=b[1],c=1;else
var
c=0;if(!c){if(typeof
a==="number")var
d=0;else
if(3===a[0])var
f=b,e=a[1],d=1;else
var
d=0;if(!d)return[7,b,a]}return cG(e,f)}return 0}var
cb=b(b7[1],[0,ca]);function
gw(a){var
b=0;function
c(c,b,a){return ex(ju(b,c),a)}return g(cb[13],c,a,b)}function
cH(e){var
b=e;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
b=b[2];continue;case
5:var
h=b[1],i=cH(b[2]),j=function(a){return bz(h,a)};return a(cb[33],j,i);case
7:var
l=b[2],m=cH(b[1]),d=cH(l),c=gw(m),n=gw(d);if(typeof
c!=="number"&&3===c[0]){var
p=c[1],q=function(a){return ao(p,a)};return a(cb[33],q,d)}var
o=b$(sU);return a(cb[6],[7,c,n],o);case
8:var
r=b[1],s=cH(b[2]),t=cH(r),u=function(e,b,a){if(b){var
c=b[1];if(a)return[0,gr(c,a[1])];var
d=c}else{if(!a)return 0;var
d=a[1]}return[0,d]};return g(cb[8],u,t,s);case
6:case
9:var
k=b$(sT);return a(cb[6],b,k)}var
f=b$(sS);return a(cb[6],b,f)}}function
sV(c,b){var
d=0;return _(function(e,d,b){return ex(cG(b,a(r[27],d,c)),e)},d,b)}function
jw(a){if(a){var
b=a[1],c=jw(a[2]);return g(k[4],sW,b,c)}return sX}function
jx(l,h,j,a){function
c(q){var
a=q;for(;;)if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
a=a[2];continue;case
3:return[5,b(h,a[1])];case
4:return[1,b(l,gq(h,a[1]))];case
5:var
r=a[2],s=b(l,gq(h,a[1]));return[2,s,c(r)];case
7:var
t=a[1],u=c(a[2]);return[3,c(t),u];case
8:var
v=a[1],w=c(a[2]);return[4,c(v),w];case
1:case
2:var
m=a[1],f=0,d=j;for(;;){if(d){var
n=d[2];if(m!==d[1]){var
f=f+1|0,d=n;continue}var
i=f}else
var
o=jw(j),p=g(k[4],sY,m,o),i=b(e[3],p);return[0,b(y[4],i)]}default:return b(e[3],sZ)}}return c(a)}function
ey(c,a){return jx(cu,function(a){var
c=af(a);return b(y[2],c)},c,a)}function
ez(c,b){if(typeof
b==="number")return 0;else{if(0===b[0]){var
e=b[3],d=b[2],g=b[1];if(typeof
d!=="number"&&9===d[0]){var
i=d[1],j=ez([0,g,c],e);return[1,ey(c,i),j]}var
h=ez([0,g,c],e);return[0,ey(c,d),h]}var
k=b[5],l=b[4],m=b[2],n=[0,b[1],c],o=function(a){return ez(n,a)},p=a(f[17],o,k),q=ey(c,l);return[2,ey(c,m),q,p]}}function
s0(b,a){return ez(b,js(1+ev(a)|0,a)[2])}function
be(f,u){var
c=u;for(;;)if(typeof
c==="number")return[0,K,1];else
switch(c[0]){case
0:var
c=c[2];continue;case
3:var
g=c[1],h=a(d[37],g,s1),v=0===h?1:1===h?2:b(e[3],s2);return[0,F(0,g,K),v];case
4:var
i=c[1];return[0,bz(i,i),1];case
5:var
j=c[2],l=c[1],m=be(f,j),n=m[2],o=m[1];if(0===n)return[0,bz(l,o),0];var
w=dl(n);Iz(k[1],e[28],s3,b9,l,aE,j,b9,o,w);return b(e[3],s4);case
6:var
x=c[1],p=be(f,c[2]),y=p[2];return[0,cA([1,x],p[1]),y];case
7:var
z=c[2],q=be(f,c[1]),A=q[2],B=q[1],r=be(f,z),C=r[1],D=go(A,r[2]);return[0,bz(B,C),D];case
8:var
E=c[2],s=be(f,c[1]),G=s[2],H=s[1],t=be(f,E),I=t[1],J=em(G,t[2]);return[0,gr(H,I),J];case
9:var
c=c[1];continue;default:return b(f,c[1])}}function
s6(o,n){var
d=o,c=n;for(;;)if(typeof
c==="number")return b(e[3],s7);else{if(0===c[0]){var
j=c[3],p=c[2],q=c[1],l=be(function(c){return function(b){return a(r[27],b,c)}}(d),p),f=l[2],h=l[1],i=bd(h),m=i[1],s=bx(i[2])?1-a(el(f),m,s5):0;if(s)return 1;if(0===j){var
t=dl(f);T(k[1],e[28],s8,es,h,t);return 0}var
d=g(r[4],q,[0,h,f],d),c=j;continue}var
u=c[4],v=c[2];be(function(b){return a(r[27],b,d)},v);be(function(b){return a(r[27],b,d)},u);return b(e[3],s9)}}function
s_(b,a){return[0,a[1],[0,b,a[2]]]}function
jy(c,a){var
b=a[1],d=a[2],e=b[1],f=dl(b[2]);return fu(k[1],c,s$,es,e,f,aE,d)}function
ta(c,b){var
d=g(k[1],c,tb,jy);return a(f[15],d,b)}var
jz=[aS,tc,aR(0)],td=[0,[0,K,0],0];function
te(a){return[0,[0,b$(a),1],[3,a]]}function
tf(c){var
a=c[1],e=c[2],f=a[2],g=a[1];return[0,[0,F(0,b(d[3],a[3]),g),f],e]}function
eA(b,a){var
c=a[1],d=b[1],e=c[2],f=c[1],g=d[2],h=d[1],i=jv(b[2],a[2]),j=go(g,e);return[0,[0,bz(h,f),j],i]}function
eB(b,a){var
c=a[1],d=b[1],e=c[2],f=c[1],g=d[2],h=d[1],i=ex(b[2],a[2]),j=em(g,e);return[0,[0,aW(h,f),j],i]}function
eC(b,e){var
f=e[2],g=e[1],c=g[2],h=g[1];if(0===c){var
k=ju(b,f);return[0,[0,bz(b,h),c],k]}var
i=bd(b),j=i[1];if(bx(i[2]))if(a(d[28],j,tg)){var
l=cG(j,f);return[0,[0,bz(b,h),c],l]}throw jz}function
th(g){var
h=g[2],i=g[1],j=i[2],k=i[1],m=bd(k),f=m[1],c=gj(m[2]);if(!a(l[24],l[2],c))if(!a(d[26],f,ti)){var
p=l[2],q=a4(f);if(a(l[24],q,p)){var
n=a(d[9],f,[1,c]),o=b(d[22],n);if(a(d[26],n,o))return 0;switch(j){case
0:return[0,[0,[0,F(0,tj,K),0],[6,c,h]]];case
1:return[0,[0,[0,F(0,o,cA([1,c],k)),j],[6,c,h]]];default:return b(e[3],tk)}}}return 0}function
tl(f){var
c=bd(f),a=c[1];if(bx(c[2])){var
e=b(d[25],a),g=0===e?tm:1===e?[0,1,2,[3,a]]:[0,0,2,[3,b(d[3],a)]];return[0,g]}return 0}function
gx(d,c){var
b=tl(c);if(b)return[0,b[1]];try{var
k=function(a){return cy(c,a[1][1])},g=a(f[33],k,d),l=[0,[0,1,g[1][2],g[2]]];return l}catch(b){b=o(b);if(b===L){var
h=cB(c);try{var
i=function(a){return cy(h,a[1][1])},e=a(f[33],i,d),j=[0,[0,0,e[1][2],e[2]]];return j}catch(a){a=o(a);if(a===L)return 0;throw a}}throw b}}function
gy(f,a){var
b=a[2],c=a[1],d=c[2],e=c[1];return f?[0,[0,e,d],b]:[0,[0,cB(e),d],b]}function
eD(q,I,H,G){var
J=G[1],K=I[1],j=[0,[0,K[1],K[2]],I[2]],i=[0,[0,J[1],J[2]],G[2]];for(;;){var
m=i[2],s=i[1],d=s[2],k=s[1],f=j[2],t=j[1],a=t[2],c=t[1],l=et(H,c)[1],g=et(H,k)[1];if(bx(g))var
h=[0,[0,[0,k,d],m]];else
if(0===a){if(0===d){var
M=eC(cB(g),[0,[0,c,a],f]),u=eB(eC(l,[0,[0,k,d],m]),M),v=u[1],j=[0,[0,c,a],f],i=[0,[0,v[1],v[2]],u[2]];continue}var
w=gx(q,l);if(w){var
n=w[1],x=n[1],N=gy(x,[0,[0,l,n[2]],n[3]]),O=x?cB(g):g,P=eC(O,[0,[0,c,a],f]),y=eB(eA(N,[0,[0,k,d],m]),P),z=y[1],j=[0,[0,c,a],f],i=[0,[0,z[1],z[2]],y[2]];continue}var
h=0}else
if(0===d)var
h=b(e[3],tn);else{var
A=gx(q,l),B=gx(q,g);if(A)if(B){var
o=B[1],C=o[1],p=A[1],D=p[1],Q=o[3],R=o[2],S=p[3],T=p[2];if(D!==C){var
U=eA(gy(C,[0,[0,g,R],Q]),[0,[0,c,a],f]),E=eB(eA(gy(D,[0,[0,l,T],S]),[0,[0,k,d],m]),U),F=E[1],j=[0,[0,c,a],f],i=[0,[0,F[1],F[2]],E[2]];continue}var
h=0,r=1}else
var
r=0;else
var
r=0;if(!r)var
h=0}if(h){var
L=h[1],V=L[1];return[0,[0,V,gw(cH(L[2]))]]}return 0}}function
eE(e,c){var
b=c[1],f=b[2],g=b[1];function
h(b){if(e){var
c=a(d[26],b,to);return c?c:a(d[26],b,tp)}return 1}return 0===f?jq(h,g):0}function
tq(a){var
f=1,b=bv(function(a){return eE(f,a)},a),c=b[1],g=b[2];if(c){var
d=c[1],h=d[2],i=d[1],e=d6(function(b){return eD(a,h,i,b)},g);return e?e[1]:a}return a}function
tr(a){return d8(function(e){var
f=1,b=bv(function(a){return eE(f,a)},e),c=b[1],g=b[2];if(c){var
d=c[1],h=d[2],i=d[1];return d6(function(b){return eD(a,h,i,b)},g)}return 0},a)}var
av=[0,jz,s_,tf,jy,ta,td,te,eA,eB,eC,th,eD,tr,tq,function(c,b){function
d(a){return eE(c,a)}return iV(d,function(d,c){var
e=c[1],f=e[1],g=d[1],h=c[2],i=e[2],j=d[2],k=gs(f);return a(s[3],g,k)?eD(b,j,g,[0,[0,f,i],h]):0},b)},eE],S=[0,eu,cE,ev,js,aE,gt,ex,cG,jv,s0,jx,sV,be,s6],ai=[0,[0,sg,by,dm],er,jm,gq,si,sr,gs,sp,so,jo,b$,jq,jp,bz,et,sx,jn,es,ss],bf=[0,dj,r6,ji,jj,r9,jh,jk,dk],eF=[0,je,di,ei,ej,jb,ja,gl,jc,eh,jd];aA(995,[0,rj,gk,eF,cD,bf,el,em,jl,ek,ai,S,se,go,av],"Micromega_plugin__Polynomial");var
dp=b(m[20][1],[0,a$]),aN=b(bc[25],[0,cy,i2]),dq=[aS,ts,aR(0)];function
dr(b,a){switch(a[0]){case
0:return g(k[1],b,tt,a[1]);case
1:return fu(k[1],b,tu,a[1],dr,a[2],dr,a[3]);default:return W(k[1],b,tv,dr,a[1],dr,a[2])}}function
tw(b,a){var
c=b[4],d=b[3],f=a[4],g=a[2],h=a[1],i=b[2],j=b[1];if(d===a[3])if(c===f){var
e=fB(j,h);return e?[0,[0,e[1],[2,i,g],d,c]]:0}throw[0,aV,tx]}function
ty(e,b,d){try{var
c=a(aN[7],d,e),f=tw(b,c[1]);if(f){c[1]=f[1];var
h=0;return h}throw[0,dq,[2,b[2],c[1][2]]]}catch(a){a=o(a);if(a===L)return g(aN[10],d,e,[0,b]);throw a}}var
eG=[aS,tz,aR(0)];function
gz(d,c,a){var
e=gk[1];if(b(aN[15],a)<e)return ty(d,c,a);throw eG}function
gA(i,c){var
j=fA(c[1]);if(j){var
k=j[1],f=k[2],g=k[1],l=aX(i);if(l){var
h=l[1][2],e=function(b){return a(d[9],b,h)};if(1===b(d[25],h))var
n=c[4],o=c[3],p=c[2],q=a(aY[16],e,f),m=[0,[0,a(aY[16],e,g),q],p,o,n];else
var
r=c[3],s=c[4],t=c[2],u=a(aY[16],e,g),m=[0,[0,a(aY[16],e,f),u],t,s,r];return[0,cA(h,i),m]}return fC([0,g,f],tA)?1:0}return 0}function
jA(a){return _(function(a,h,g){var
c=a[2],e=a[1],f=b(d[25],g);if(0===f)throw[0,aV,tC];return 1===f?[0,e,c+1|0]:[0,e+1|0,c]},tB,a)}function
jB(a,f){var
b=a[3],c=a[1],g=a[2],d=jA(c),h=d[2],i=d[1],j=[0,f];switch(g){case
0:var
e=[0,[0,b],[0,b]];break;case
1:var
e=[0,[0,b],0];break;default:throw ek}return gA(c,[0,e,j,h,i])}function
tD(d){var
c=b(aN[1],1000);function
e(b,a){return[0,a,b]}var
f=a(m[17][13],e,d),h=dp[1];function
i(e,d){var
f=d[2],g=d[1],b=jB(g,f);if(typeof
b==="number"){if(0===b)throw[0,dq,[0,f]];return e}gz(b[1],b[2],c);var
h=g[1];return _(function(c,b,d){return a(dp[4],b,c)},e,h)}return[0,c,g(m[17][15],i,h,f)]}function
jC(a){var
b=a[1],c=0;function
d(c,b,a){return[0,[0,c,b[1]],a]}return g(aN[14],d,b,c)}function
gB(e,c){var
f=c[2],g=e[2],i=c[1],j=e[1];if(a(d[31],g,tE))if(a(d[31],f,tF)){var
h=a(d[9],tG,f),b=gf(a(d[9],tH,g),j,h,i);return[0,b,jA(b)]}throw[0,aV,tI]}function
tJ(i,c){var
h=c[1];function
j(k,q,j){var
a=q[1],f=j[3],g=j[2],h=j[1],c=au(i,k);if(0===c[0])if(0===c[1])return[0,h,[0,[0,k,a],g],f];function
e(d,b){return b?[0,[0,c,k,[0,[0,[0,b[1]],0],a[2],a[3],a[4]]],d]:d}var
l=a[1],m=l[2],n=l[1];if(1===b(d[25],c)){var
o=e(f,m);return[0,e(h,n),g,o]}var
p=e(f,n);return[0,e(h,m),g,p]}var
e=g(aN[14],j,h,tK),k=e[3],l=e[2],n=e[1],o=b(aN[15],c[1]),f=b(aN[1],o);function
p(a){return g(aN[10],f,a[1],[0,a[2]])}a(m[17][11],p,l);function
q(e){function
c(g){var
h=g[3],j=g[1],k=e[3],l=e[1],p=g[2],q=e[2],r=h[1],s=b(aY[7],k[1][1]),t=b(aY[7],r[1]),u=b(d[3],j),v=a(d[9],t,u),w=a(d[9],s,l),x=a(d[1],w,v),m=gB([0,q,l],[0,p,b(d[3],j)]),n=m[2],o=[0,[0,[0,x],0],[1,i,k[2],h[2]],n[2],n[1]],c=gA(m[1],o);if(typeof
c==="number"){if(0===c)throw[0,dq,o[2]];return 0}return gz(c[1],c[2],f)}return a(m[17][11],c,k)}a(m[17][11],q,n);return[0,f,a(dp[6],i,c[2])]}function
tM(e,q,C,B,c){var
r=au(e,q),f=b(aN[15],c[1]),s=b(aN[1],f),g=c[1];function
h(g,D){var
h=D[1],c=au(e,g);if(0===c[0])if(0===c[1])var
i=[0,g,h],j=1;else
var
j=0;else
var
j=0;if(!j)var
k=a(d[30],c,tL)?b(d[3],r):r,l=b(d[15],c),m=gB([0,q,k],[0,g,l]),n=m[2],u=n[2],v=n[1],w=m[1],x=a(d[9],C,k),o=function(b){var
c=a(d[9],b,l);return a(d[1],x,c)},p=h[1],y=p[1],z=a(aY[16],o,p[2]),A=[0,a(aY[16],o,y),z],i=[0,w,[0,A,[1,e,B,h[2]],u,v]];var
t=i[2],f=gA(i[1],t);if(typeof
f==="number"){if(0===f)throw[0,dq,t[2]];return 0}return gz(f[1],f[2],s)}a(aN[12],h,g);return[0,s,a(dp[6],e,c[2])]}var
G=b(tN[1],[0,a$]);function
jD(x,w,c){function
f(p,n,y){var
t=[0,tO,K],i=_(function(e,c,b){var
f=e[2],g=e[1];try{var
h=a(G[23],c,x),i=a(d[6],h,b),j=[0,a(d[1],g,i),f];return j}catch(a){a=o(a);if(a===L)return[0,g,F(c,b,f)];throw a}},t,p),q=i[2],f=i[1],r=au(w,q),g=n[1][1];function
c(b){var
c=a(d[4],b,f);return a(d[9],c,r)}var
j=g[2],l=g[1],m=b(d[25],r);if(0===m)var
h=fC(g,f)?tP:b(e[3],tQ);else
if(1===m)var
u=a(aY[16],c,j),h=[0,a(aY[16],c,l),u];else
var
v=a(aY[16],c,l),h=[0,a(aY[16],c,j),v];var
s=fB(y,h);if(s)return s[1];var
z=b(d[40],f);fu(k[1],e[28],tS,b9,p,z,b9,q);C(k[1],e[28],tT,hY,n[1][1]);return b(e[3],tU)}return g(aN[14],f,c,tR)}function
t3(g,k,j,d,c){function
e(c,f){var
l=b(k,c);try{var
r=function(a){return a[1][1]!==g?1:0},d=a(m[17][27],r,l)[1],i=d[1],s=e(tM(i,d[2],d[3],d[4],c),[0,[0,i,c],f]);return s}catch(d){d=o(d);if(d===L){var
n=b(j,c);try{var
p=function(a){return a[1]!==g?1:0},h=a(m[17][27],p,n)[1],q=e(tJ(h,c),[0,[0,h,c],f]);return q}catch(a){a=o(a);if(a===L)return[0,[0,c,f]];throw a}}throw d}}return e(d,c)}function
jE(d,c,b,a){try{var
e=t3(d,c,b,tD(a),0);return e}catch(a){a=o(a);if(a[1]===dq)return[1,a[2]];throw a}}function
jF(c){var
e=c[2],f=[0,0,jC(c)];function
h(x,w){var
e=w[2],c=0,h=0,i=0,f=0,G=w[1];for(;;){if(e){var
j=e[2],n=e[1],a=n[2],o=n[1],p=aX(o);if(p){var
l=p[1],q=l[3],y=l[2];if(x===l[1]){var
k=function(b){return function(a,c){return c?[0,b[4]+b[3]|0,a]:a}}(a),r=a[1],s=r[2],t=r[1];if(1===b(d[25],y)){var
z=k(f,s),e=j,c=[0,[0,q,a],c],h=k(h,t),f=z;continue}var
A=k(f,t),e=j,c=[0,[0,q,a],c],h=k(h,s),f=A;continue}var
e=j,c=[0,[0,o,a],c],i=(a[4]+a[3]|0)+i|0;continue}var
e=j,c=[0,[0,K,a],c],i=(a[4]+a[3]|0)+i|0;continue}var
u=b(m[17][1],h),B=0,C=function(b,a){return b+a|0},D=g(m[17][15],C,B,h),v=b(m[17][1],f),E=0,F=function(b,a){return b+a|0};return[0,[0,[0,x,i+v*D+u*g(m[17][15],F,E,f)-v*u],G],c]}}var
i=g(dp[15],h,e,f)[1];function
j(b,a){return B.caml_float_compare(b[2],a[2])}return a(m[17][39],j,i)}function
jG(b){var
c=b[1];if(c){var
e=b[2];if(e)return a(d[26],c[1],e[1])}return 0}function
jH(b,g){var
a=g;for(;;){var
c=aX(a);if(c){var
d=c[1],e=d[3],f=d[1];if(f===b)return[0,1,e];if(f<b){var
a=e;continue}return[0,0,a]}return[0,0,K]}}function
jI(I){var
l=jC(I),J=0;function
K(c,e){var
b=e[2],f=b[1],g=f[1],j=e[1];if(g){var
h=f[2];if(h){var
i=g[1];return a(d[26],i,h[1])?[0,[0,j,i,b[2],b[4]+b[3]|0],c]:c}}return c}var
n=g(m[17][15],K,J,l),b=n;for(;;){if(b){var
o=b[2],c=b[1],p=c[1],x=c[4],y=c[3],z=c[2],q=aX(p);if(!q){var
b=o;continue}var
r=q[1],A=r[1];if(!bx(r[3])){var
b=o;continue}var
k=[0,[0,A,p,z,y,x]]}else
var
k=0;if(k)var
h=[0,k[1]];else{var
e=n;a:for(;;){if(e){var
f=e[1],w=f[1],s=w,E=e[2],F=f[4],G=f[3],H=f[2];for(;;){var
t=aX(s);if(t){var
u=t[1],v=u[1],D=u[3],B=0,C=function(d){return function(a,b){var
c=b[2];return jH(d,b[1])[1]?jG(c[1])?a+1|0:a:a}}(v);if(2!==g(m[17][15],C,B,l)){var
s=D;continue}var
j=[0,v]}else
var
j=0;if(!j){var
e=E;continue a}var
h=[0,[0,j[1],w,H,G,F]];break}}else
var
h=0;break}}if(h){var
i=h[1];return[0,[0,[0,i[1],i[2],i[3],i[4]],0],0]}var
L=0,M=function(u,e){var
r=e[1],n=r,m=l,i=u,v=e[4],w=e[3],x=e[2];b:for(;;){var
o=aX(n);if(o){var
p=o[1],q=p[1],c=m,b=0,a=0,s=p[3],t=v-1|0;for(;;){if(c){var
f=c[2],j=c[1],d=j[2],g=d[3]+d[4]|0,k=jH(q,j[1]),h=k[2];if(0===k[1]){var
c=f,b=b+g|0,a=[0,[0,h,d],a];continue}if(jG(d[1])){var
c=f,b=b+g|0,a=[0,[0,h,d],a];continue}var
c=f,b=(b+g|0)+t|0,a=[0,[0,h,d],a];continue}var
n=s,m=a,i=[0,[0,[0,q,r,x,w],b],i];continue b}}return i}},N=g(m[17][15],M,L,n),O=function(b,a){return a$(b[2],a[2])};return a(m[17][39],O,N)}}function
t4(h,d){var
i=0;function
j(c,b){var
d=i5(b[1]);return a(e[1][5],c,d)}var
c=g(m[17][15],j,i,d),f=jE(c,jI,jF,[0,[0,F(c,t6,h),0,t5],d]);if(0===f[0]){var
l=f[1][1];try{var
p=[0,jD(G[1],c,l[1])];return p}catch(c){c=o(c);if(b(cI[18],c)){var
n=b(d7[1],c);a(k[2],t7,n);return 0}throw c}}return 0}function
t8(w){var
j=jE(e[8],jI,jF,w);if(0===j[0]){var
i=j[1][2],h=G[1];for(;;){if(i){var
r=i[1],s=r[1],x=i[2],k=jD(h,s,r[2][1]),m=k[1];if(m){var
n=k[2],c=m[1];if(n){var
o=n[1];if(a(d[29],c,tV))if(a(d[29],tW,o))var
f=tX,l=1;else
var
l=0;else
var
l=0;if(!l)var
t=b(d[22],o),u=b(d[24],c),f=a(d[29],u,t)?b(d[24],c):c}else
var
f=a(d[29],c,tY)?tZ:b(d[24],c)}else{var
p=k[2];if(p)var
q=p[1],v=b(d[22],q),f=a(d[29],t0,v)?t1:b(d[22],q);else
var
f=t2}var
i=x,h=g(G[4],s,f,h);continue}var
y=function(c,b,a){return F(c,b,a)};return[0,g(G[12],y,h,K)]}}return[1,j[1]]}function
cc(b,a){return gB(b,a)[1]}function
eH(b,a){if(0===b)if(0===a)return 0;return 1}function
jJ(r,q,p){var
j=p[2],k=p[1],l=q[2],m=q[1],n=j[3],f=j[2],g=j[1],o=l[3],h=l[2],i=l[1],c=au(r,i),e=au(r,g),U=0===c[0]?0===c[1]?1:0:0;if(!U){var
V=0===e[0]?0===e[1]?1:0:0;if(!V){var
s=b(d[25],e);if(-1===ft(b(d[25],c),s)){var
t=b(d[15],e),u=a(d[9],n,t),v=b(d[15],c),w=a(d[9],o,v),x=a(d[1],w,u),y=eH(h,f),z=[0,g,b(d[15],e)],A=[0,cc([0,i,b(d[15],c)],z),y,x],B=[0,k,b(d[15],e)];return[0,[0,cc([0,m,b(d[15],c)],B),A]]}if(0===h){var
C=a(d[9],n,t9),D=a(d[9],c,e),E=b(d[3],D),F=a(d[9],o,E),G=a(d[1],F,C),H=eH(h,f),I=a(d[9],c,e),J=[0,cc([0,i,b(d[3],I)],[0,g,t_]),H,G],K=a(d[9],c,e);return[0,[0,cc([0,m,b(d[3],K)],[0,k,t$]),J]]}if(0===f){var
L=a(d[9],o,ua),M=a(d[9],e,c),N=b(d[3],M),O=a(d[9],n,N),P=a(d[1],O,L),Q=eH(h,f),R=a(d[9],e,c),S=[0,cc([0,g,b(d[3],R)],[0,i,ub]),Q,P],T=a(d[9],e,c);return[0,[0,cc([0,k,b(d[3],T)],[0,m,uc]),S]]}return 0}}return 0}var
jK=[0,function(y,c){function
f(c){switch(c[0]){case
0:var
k=c[1],z=a(m[17][7],y,k);return[0,[0,F(k,uh,K),z],0];case
1:var
A=c[3],B=c[1],C=f(c[2]),D=f(A),v=0,w=function(a,c){function
b(a,d){var
b=jJ(B,c,d);return b?[0,b[1],a]:a}return g(m[17][15],b,a,D)};return g(m[17][15],w,v,C);default:var
E=c[2],G=f(c[1]),H=f(E),I=a(m[18],G,H),x=function(b,d){var
c=d[2],e=d[1];if(0===b[0]){var
f=b[1],a=jB(c,0);return typeof
a==="number"?0===a?[1,[0,e,c]]:[0,f]:[0,[0,[0,e,c,a[1],a[2]],f]]}return b},i=g(m[17][15],x,ud,I);if(0===i[0]){var
J=i[1],L=function(k,j){if(0===k[0]){var
t=k[1],l=j[2],m=j[1],n=j[4][1],x=t[2],y=t[1],u=n[2],v=n[1],o=function(e,c,b){if(c){var
f=c[1][3];if(b){var
d=b[1];return a(e,f,d)?[0,[0,m,l,d]]:c}return c}return b?[0,[0,m,l,b[1]]]:0},c=o(d[29],y,v),f=o(d[30],x,u);if(c)if(f){var
g=f[1],h=g[2],p=g[1],i=c[1],q=i[1],w=i[2];if(a(d[29],i[3],g[3]))return[0,[0,c,f]];var
r=aX(h[1]);if(r){var
s=jJ(r[1][1],[0,q,w],[0,p,h]);return s?[1,s[1]]:b(e[3],ue)}return[1,[0,cc([0,q,ug],[0,p,uf]),h]]}return[0,[0,c,f]]}return k},j=g(m[17][15],L,ui,J);if(0===j[0]){var
l=j[1],h=l[2],n=l[1];if(n){var
o=n[1],p=o[2],q=o[1];if(h){var
r=h[1];return[0,[0,q,p],[0,[0,r[1],r[2]],0]]}var
t=p,s=q}else{if(!h)return 0;var
u=h[1],t=u[2],s=u[1]}return[0,[0,s,t],0]}return[0,j[1],0]}return[0,i[1],0]}}return f(c)},eH],eI=[0,t8,t4];aA(1001,[0,[0,G[1],G[2],G[3],G[4],G[5],G[6],G[7],G[8],G[9],G[10],G[11],G[12],G[13],G[14],G[15],G[16],G[17],G[18],G[19],G[20],G[21],G[22],G[23],G[24],G[25],G[26]],eI,dr,jK,eG],"Micromega_plugin__Mfourier");function
gC(c,b){var
a=b[2];return a?c===a[1]?1:0:0}function
ds(b,a){var
c=a[1]<=b?1:0,d=c?1-gC(b,a):c;return d}function
eJ(a){return[0,a,0]}function
jL(b,a){return[0,a[1],[0,b]]}function
uk(c,b){return ee(function(e,b){return a(d[28],b,ul)?0:ds(e,c)},b)}function
um(c,b){try{var
a=bd(b),d=a[1],e=uk(c,a[2])?[0,d]:0;return e}catch(a){a=o(a);if(a===L)return 0;throw a}}function
gD(f,d,c){try{var
g=a(r[27],d,c);return g}catch(a){a=o(a);if(a===L)return b(e[3],f);throw a}}function
jM(j,f,c){var
k=gD(uz,f,j),i=au(c,k);if(a(d[26],i,ur))var
h=b(e[3],us);else
var
l=a(d[9],ut,i),h=ao(l,F(f,uv,F(c,uu,k)));var
o=a(r[7],f,j);function
m(b){var
e=au(c,b);return a(d[26],e,uw)?b:gf(e,h,uy,F(c,ux,b))}var
n=a(r[33],m,o);return g(r[4],c,h,n)}function
uB(b,a){return 0}var
uC=[0,b(r[10],uB)];b(f8[1],uC);function
gE(D,h,k,C){var
c=C;for(;;){if(!D){var
G=dh(a(r[27],h,c));if(a(d[30],G,uE))return[0,c,0]}var
s=gD(uq,h,c),t=um(k,s);if(t)var
f=[0,[0,t[1]]];else{var
p=bd(s)[2];for(;;){var
q=aX(p);if(q){var
l=q[1],m=l[1],x=l[3];if(a(d[27],l[2],un)){if(ds(m,k)){var
p=x;continue}var
j=[0,m,-1]}else
var
j=[0,m,1]}else
var
j=b(e[3],uo);var
n=j[1],z=j[2],A=a(r[7],h,c),y=0,v=a(r[39],k[1],A),w=function(p,q){return function(e,g,c){if(gC(e,k))return c;var
j=au(q,g),m=a(d[6],[0,p],j);if(a(d[27],m,up)){var
n=dh(g),o=a(d[9],n,j),f=b(d[15],o);if(c){var
h=c[1],i=h[2],l=h[1];return a(d[27],i,f)?c:a(d[27],f,i)?[0,[0,e,f]]:bZ(l,e)?c:[0,[0,e,f]]}return[0,[0,e,f]]}return c}}(z,n),o=g(r[13],w,v,y);if(o)var
u=o[1],f=[1,u[1],n,u[2]];else
var
f=[0,[1,n]];break}}if(0===f[0]){var
i=f[1];if(typeof
i==="number")throw[0,jN,uD];else{if(0===i[0])return[0,c,i];var
E=i[1],B=dh(a(r[27],h,c)),F=a(d[30],B,uA)?c:jM(c,h,E);return[0,F,i]}}var
c=jM(c,f[1],f[2]);continue}}function
gF(d,e,c){var
b=_(function(d,c,b){try{var
f=aW(ao(b,a(r[27],c,e)),d);return f}catch(a){a=o(a);if(a===L)return aW(F(c,b,K),d);throw a}},K,c);return g(r[4],d,b,e)}function
jO(j,b,i,h,g){var
f=gE(j,b,h,gF(b,g,i)),c=f[2],e=f[1];return typeof
c==="number"?[1,e,0]:0===c[0]?a(d[30],c[1],uF)?[1,e,0]:[0,F(b,uJ,F(0,uI,ao(uH,gD(uG,b,e))))]:[1,e,[0,c[1]]]}function
eK(d){try{var
e=s[1],h=function(c,b){var
d=gh(b[1]);return a(s[7],c,d)},i=g(f[20],h,e,d),j=b(s[24],i),c=j}catch(a){a=o(a);if(a!==L)throw a;var
c=0}return 1+c|0}function
eL(m,l){var
c=0,a=m,h=r[1],f=l,e=0;for(;;){if(f){var
j=f[2],i=f[1];switch(i[2]){case
0:var
n=i[1],k=F(0,b(d[3],i[3]),n),o=ao(uK,k),p=g(r[4],a+1|0,[0,c,0],h),q=g(r[4],a,[0,c,1],p),c=c+1|0,s=[0,[0,a,k],[0,[0,a+1|0,o],e]],a=a+2|0,h=q,f=j,e=s;continue;case
1:var
t=i[1],u=[0,[0,a,F(0,b(d[3],i[3]),t)],e],v=g(r[4],a,[0,c,1],h),c=c+1|0,a=a+1|0,h=v,f=j,e=u;continue;default:throw ek}}return[0,a,h,e]}}function
gG(c,a){function
b(b,d,a){return ds(b,c)?a:F(b,dh(d),a)}return g(r[13],b,a,K)}function
eM(E,D,f,C){var
h=D,g=C;for(;;){var
n=F(0,uL,gG(f,g));if(h){var
t=h[1],u=t[2],A=h[2],B=t[1],c=A,o=ef(n,u),e=[0,B,u],b=0;for(;;){var
j=e[2],k=e[1];if(c){var
p=c[2],q=c[1],l=q[2],r=q[1],s=ef(n,l);if(a(d[29],s,o)){var
c=p,o=s,e=[0,r,l],b=[0,[0,k,j],b];continue}var
c=p,e=[0,k,j],b=[0,[0,r,l],b];continue}var
m=[0,[0,[0,k,j],b]];break}}else
var
m=0;if(m){var
v=m[1],w=v[2],x=v[1],y=x[1],G=x[2],i=jO(E,y,G,jL(y,f),g);if(0===i[0])return[1,i[1]];var
z=i[1],H=i[2];if(w){var
h=w,g=z;continue}return[0,[0,f,z,H]]}return[0,[0,f,g,0]]}}function
jP(c){var
e=eK(c),f=eL(e,c),h=f[3],j=f[2],i=r[1],g=eM(0,h,eJ(e),i);if(0===g[0])return 0;var
k=g[1];return[0,cC(_(function(g,f,c){var
e=a(r[27],f,j),h=e[1],i=e[2]?c:b(d[3],c);return F(h,i,g)},K,k))]}function
jQ(a){var
b=eK(a),e=eL(b,a)[3],f=r[1],c=eM(0,e,eJ(b),f);if(0===c[0]){var
d=c[1];return[0,gG(d[1],d[2])]}return 0}function
jR(e,c){var
a=eK(c),k=eL(a+1|0,c)[3];function
f(f,e){var
a=e[2];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=a[1],g=f?c:b(d[3],c);return[0,g]}return 0}}var
l=r[1],g=eM(0,k,eJ(a),l);if(0===g[0]){var
h=g[1],i=h[2],j=h[1],m=f(1,gE(1,a,j,gF(a,i,e)));return[0,[0,f(0,gE(1,a,j,gF(a,i,cB(e)))),m]]}return 0}function
eN(c){var
e=b(d[22],c);return a(d[4],c,e)}var
jS=[aS,uM,aR(0)];function
uN(c,d,b){var
e=F(c,uO,K);try{var
f=function(a,f){var
b=ds(a,d);if(b){if(cy(e,f))throw[0,jS,a];var
c=0}else
var
c=b;return c};a(r[12],f,b);var
g=0;return g}catch(a){a=o(a);if(a[1]===jS)return[0,a[2]];throw a}}function
jT(i,z,n,s,h,m,g){var
j=g[2],k=g[1],c=eN(bd(j)[1]);if(a(d[26],c,uR))return 0;var
p=a(d[9],uT,uS);if(a(d[27],c,p))var
e=a(d[9],uU,c),q=b(d[20],e)?a(d[4],e,uV):b(d[22],e),l=q;else
var
l=u1;function
t(e){var
b=eN(e),f=a(d[4],uW,c);if(a(d[29],b,f)){var
g=a(d[4],uX,c);return a(d[9],b,g)}var
h=a(d[4],uY,b);return a(d[9],h,c)}var
u=[0,t,[0,function(b){return eN(a(d[6],l,b))},0]];function
v(n){var
k=F(0,uP,j),c=b(i9(function(e,c){var
b=1-ds(e,h);if(b){var
f=eN(c);return a(d[31],f,uQ)}return b}),k),e=c[2],f=c[1],g=bx(f)?[0,e]:[0,_(function(a,d,c){var
b=uN(d,h,m);return b?F(b[1],c,a):a},e,f)];if(g)var
l=g[1],i=_(function(d,c,a){return F(c,b(n,a),d)},K,l);else
var
i=K;return cC(i)}var
w=a(f[17],v,u);function
x(e){var
c=av[6];return _(function(h,e,c){try{var
g=a(r[27],e,s),l=g[1],m=g[2]?c:b(d[3],c),n=a(r[27],l,i),p=ge(m),q=a(av[10],p,n),f=q}catch(b){b=o(b);if(b!==L)throw b;var
j=a(r[27],e,i),k=ge(c),f=a(av[10],k,j)}return a(av[9],h,f)},c,e)}var
y=a(f[17],x,w);return iS(function(g){var
d=b(av[11],g);if(d){var
e=d[1],f=e[2],c=e[1];if(0===c[2])return[0,[0,k,[0,c,f]]];var
h=F(0,uZ,n),i=ef(c[1],h);return a(el(1),i,u0)?0:[0,[0,k,[0,c,f]]]}return 0},y)}function
jU(m){var
h=b(f[46],m)[1],i=eK(h),c=eL(i,h),l=c[2],q=c[3],u=c[1],v=a(f[17],av[3],m),n=[0,0,r[1]];function
p(a,c){var
b=a[1];return[0,b+1|0,g(r[4],b,c,a[2])]}var
s=[0,0],w=g(f[20],p,n,v)[2];function
t(h,u,c,m){s[1]++;if(0===m[0]){var
v=m[1],f=v[2],j=v[1],w=gG(j,f);if(0===(s[1]%2|0))var
H=0,I=function(c,b,a){return a?[0,a[1]]:jT(h,u,w,l,j,f,[0,c,b])},n=g(r[13],I,f,H);else
var
J=0,K=function(t,s,c){var
g=jT(h,u,w,l,j,f,[0,t,s]);if(g){var
e=g[1];if(c){var
i=c[1],k=i[2],m=k[2],n=k[1],o=e[2],p=o[2],q=o[1],v=n[2],x=n[1],y=i[1],z=q[2],A=q[1],B=e[1],C=b(S[1],m),D=b(S[1],p),E=a(d[27],D,C)?[0,B,[0,[0,A,z],p]]:[0,y,[0,[0,x,v],m]];return[0,E]}var
r=e}else{if(!c)return 0;var
r=c[1]}return[0,r]},n=g(r[13],K,f,J);if(n){var
x=n[1],y=x[2],z=y[2],A=y[1],B=A[2],C=A[1],O=x[1];if(0===B)return[0,[0,c,[9,z],0]];var
p=jL(c,j),i=jO(1,c,C,p,f);if(0===i[0])var
D=[1,i[1]];else{var
M=i[2],N=i[1];if(gC(c,p))var
q=[0,p[1],0];else
var
F=a(k[4],uj,c),q=b(e[3],F);var
D=[0,[0,q,N,M]]}var
E=t(g(r[4],c,[0,[0,C,B],[2,c]],h),[0,O],c+1|0,D);return E?[0,[0,c,[9,z],E[1]]]:0}return 0}var
P=0,Q=cC(m[1]),G=0;return[0,[0,c,_(function(i,e,c){try{var
g=a(r[27],e,l),k=g[2],m=a(r[27],g[1],h)[2],n=k?c:b(d[3],c),p=a(S[8],n,m),f=p}catch(b){b=o(b);if(b!==L)throw b;var
j=a(r[27],e,h)[2],f=a(S[8],c,j)}return a(S[7],i,f)},G,Q),P]]}var
x=r[1],j=t(w,0,u,eM(1,q,eJ(i),x));return j?[0,j[1]]:0}aA(1003,[0,jR,jQ,jP,jU],"Micromega_plugin__Simplex");function
a5(f,c){if(typeof
c==="number")return a(e[55],f,u2);else
switch(c[0]){case
0:var
m=b(d[40],c[1]);return a(e[55],f,m);case
1:return g(k[1],f,u3,c[1]);case
2:return C(k[1],f,u4,a5,c[1]);case
3:var
h=c[1];return W(k[1],f,u5,a5,h[1],a5,h[2]);case
4:var
i=c[1];return W(k[1],f,u6,a5,i[1],a5,i[2]);case
5:var
j=c[1];return W(k[1],f,u7,a5,j[1],a5,j[2]);default:var
l=c[1];return T(k[1],f,u8,a5,l[1],l[2])}}function
cJ(e,c){switch(c[0]){case
0:return g(k[1],e,u9,c[1]);case
1:return g(k[1],e,u_,c[1]);case
2:return g(k[1],e,u$,c[1]);case
3:var
f=b(d[40],c[1]);return g(k[1],e,va,f);case
4:var
h=b(d[40],c[1]);return g(k[1],e,vb,h);case
5:var
i=b(d[40],c[1]);return g(k[1],e,vc,i);case
6:return C(k[1],e,vd,a5,c[1]);case
7:return a(k[1],e,ve);case
8:return W(k[1],e,vf,a5,c[1],cJ,c[2]);case
9:return W(k[1],e,vg,cJ,c[1],cJ,c[2]);default:return W(k[1],e,vh,cJ,c[1],cJ,c[2])}}aA(1004,[0,a5,cJ],"Micromega_plugin__Sos_types");var
bT=[0,1],vi=0,vj=n[8],vl=0;function
vm(a){return[1,b(ag[1],a)]}var
eO=[0,y[2],vm,vl,vk,vj,aL],vp=ag[2],gH=[0,function(a){return[0,b(y[2],a),0]},vp,vo,vn,aT,at];function
jV(a,i){var
b=i;for(;;){var
d=a[6],e=a[5],f=a[4],g=a[3],h=function(b,c,d,e){return function(a){return iu(e,d,c,b,a)}}(d,e,f,g),c=function(c){function
b(a){if(typeof
a!=="number")switch(a[0]){case
3:var
d=a[1],e=b(a[2]);return c([3,b(d),e]);case
4:var
f=a[1],g=b(a[2]);return c([4,b(f),g])}return c(a)}return b}(h)(b);if(X(c,b))return c;var
b=c;continue}}function
eP(a){var
e=a[2],c=bd(a[1]),f=c[2];return[0,f,e,b(d[3],c[1])]}function
vs(c){var
p=s[1];function
q(d,c){var
b=gh(c[1]);return a(s[7],d,b)}var
r=g(f[20],q,p,c),t=0;function
u(k,j){var
a=0;function
d(b,a){return[0,au(k,a[1]),b]}var
e=g(f[20],d,a,c),h=[1,l[1]],i=b(f[9],e);return[0,[0,b_([0,[1,l[1]],[0,[1,l[1]],i]]),0,h],j]}var
v=g(s[15],u,r,t),i=0;function
j(c,a){return[0,b(d[3],a[3]),c]}var
k=g(f[20],j,i,c),m=[1,l[1]],n=b(f[9],k),o=[0,b_([0,[1,l[1]],[0,[1,l[2]],n]]),0,m],w=[1,l[2]],x=1;function
y(a){return jl(a)?[1,l[2]]:[1,l[1]]}var
z=a(f[17],y,c),A=[0,b_([0,[1,l[1]],[0,[1,l[2]],z]]),x,w],B=[0,o,v];function
h(e,d){var
b=e,a=d;for(;;){if(a){var
c=a[2];if(0===a[1][2]){var
b=b+1|0,a=c;continue}var
f=h(b+1|0,c),g=1;return[0,[0,gd(b+1|0,function(a){return vr},K),g,vq],f]}return 0}}var
C=[0,A,h(1,c)],D=a(e[26],C,B),E=[1,l[1]];return[0,[0,b_([0,[1,l[1]],[0,[1,l[2]],0]]),1,E],D]}function
gI(c){if(bT[1])return jP(c);var
d=b(eI[1],c);if(0===d[0])return 0;var
e=a(jK[1],c,d[1]);return[0,cC(b(m[17][5],e)[1])]}function
vt(a){if(bT[1])return jQ(a);var
c=b(eI[1],a);return 0===c[0]?[0,c[1]]:0}function
vw(g){try{var
a=gI(g);return a}catch(a){a=o(a);if(a===ek){var
h=vs(g);try{var
c=vt(h);if(c)var
d=c[1],i=aX(d)?[0,cC(i7(2,F(1,vu,d)))]:b(e[3],vv),f=i;else
var
f=0;return f}catch(a){a=o(a);if(b(cI[18],a))return 0;throw a}}throw a}}function
gJ(a){var
b=[0,0,r[1]];function
c(a,c){var
b=a[1];return[0,b+1|0,g(r[4],b,c,a[2])]}return g(m[17][15],c,b,a)[2]}function
gK(e){var
c=b(m[17][mK],e),f=c[2],d=vw(c[1]);if(d){var
g=d[1],h=gJ(f);return[0,a(S[12],h,g)]}return 0}var
dt=[aS,vy,aR(0)];function
gL(k){var
f=k[2],g=k[1],h=g[3],i=g[2],j=g[1];if(aX(j)){var
l=gj(j),c=[1,l];if(a(d[32],c,vz))return[2,g,f];var
m=a(d[12],h,c),n=b(d[25],m);if(a(D[2],n,0)){if(1<=b(d[25],c)){var
o=a(d[9],h,c);return[2,[0,cA(c,j),i,o],[6,l,f]]}throw[0,aV,vA]}switch(i){case
0:return[0,[9,f]];case
1:var
p=a(d[9],h,c),q=b(d[24],p);return[1,[0,cA(c,j),i,q],[9,f]];default:return b(e[3],vB)}}return a(el(i),vC,h)?0:[0,f]}function
jW(h,f,a){var
c=0;function
d(c,d){var
e=b(f,d);if(e){var
a=b(h,e[1]);if(typeof
a==="number")return c;else
switch(a[0]){case
0:throw[0,dt,a[1]];case
1:return[0,[0,a[1],a[2]],c];default:return[0,[0,a[1],a[2]],c]}}return[0,d,c]}return g(m[17][15],d,c,a)}function
jX(c){return d8(function(f){var
e=bv(function(l){var
c=l[1],g=c[2],h=c[1];function
i(b){var
c=a(d[26],b,vD);return c?c:a(d[26],b,vE)}if(0===g){var
j=a(ai[13],i,h),k=function(e){function
c(d){var
c=b(ai[9],d[1][1]);return c?c:a(ai[10],e,d[1][1])}return a(m[17][21],c,f)},e=a(m[17][61],k,j);return e?[0,e[1]]:0}return 0},f),h=e[1],j=e[2];if(h){var
i=h[1];return d6(g(av[12],c,i[2],i[1]),j)}return 0},c)}function
gM(b){return a(av[15],0,b)}function
jY(c,b){if(1===c)return b;var
d=jY(c-1|0,b);return a(av[8],b,d)}function
vF(d,c){if(!b(eF[6],c))if(!b(eF[3],c))try{var
e=b(av[7],vG),f=function(e,c,b){var
f=jY(c,a(r[27],e,d));return a(av[8],f,b)},h=[0,g(eF[1],f,c,e)];return h}catch(a){a=o(a);if(a===L)return 0;throw a}return 0}function
du(g,f,d){b(ai[1][1],0);var
c=b(m[17][1],d),h=a(e[6],g,ft(c,g));gk[1]=a(e[6],c,h);function
i(e){var
g=e[1];switch(e[2]){case
0:var
c=0;break;case
1:throw[0,aV,vx];case
2:var
c=2;break;default:var
c=1}function
d(c){switch(c[0]){case
0:var
g=b(f[2],c[1]);return b(bf[1],g);case
1:var
h=b(ag[3],c[1]);return b(bf[2],h);case
2:var
i=c[1],j=d(c[2]),k=d(i);return a(bf[3],k,j);case
3:var
l=c[1],m=d(c[2]),n=b(bf[5],m),o=d(l);return a(bf[3],o,n);case
4:var
p=c[2],q=d(c[1]),r=d(p);return a(bf[4],q,r);case
5:var
s=d(c[1]);return b(bf[5],s);default:var
t=c[2],u=d(c[1]),v=b(ag[4],t),e=function(c){if(a(D[2],c,0)){var
d=b(f[2],f[4]);return b(bf[1],d)}var
g=e(c-1|0);return a(bf[4],u,g)};return e(v)}}return[0,d(g),c]}var
j=a(m[17][68],i,d);function
k(c,a){var
d=a[2];return[0,[0,b(ai[2],a[1]),d],[1,c]]}return a(m[17][13],k,j)}function
jZ(c){function
f(a){return b(ai[9],a[1][1])}if(a(m[17][21],f,c))return c;var
h=cD[1];function
i(c,a){var
d=b(ai[16],a[1][1]);function
e(c,a,b){return[0,a]}return g(cD[38],e,c,d)}var
j=g(m[17][15],i,h,c);function
k(d,c,a){var
e=b(ai[5],d);return[0,[0,[0,b(ai[5],c),1],[4,e]],a]}var
d=g(cD[12],k,j,c),l=s[1];function
n(d,c){var
e=b(ai[7],c[1][1]);return a(s[7],d,e)}var
o=g(m[17][15],n,l,d);function
p(e,d){var
c=b(ai[3],e);return[0,[0,[0,a(ai[14],c,c),1],[4,c]],d]}var
e=g(s[15],p,o,d),q=iQ(av[8],e),r=a(m[18],e,q),t=b(av[2],vH);return a(m[17][68],t,r)}function
j0(i,h){var
c=jX(du(i,gH,h)),j=gM(c),k=jZ(c),l=a(m[18],k,j);function
n(a){var
b=a[1],c=a[2];return[0,eP([0,b[1],b[2]]),c]}var
d=a(m[17][68],n,l),o=0;function
p(d,c){var
f=b(S[2],c[2]);return a(e[6],d,f)}var
q=g(m[17][15],p,o,d),r=a(gN[53],0,q),f=gK(d);return f?[0,C(S[11],f7,y[5],r,f[1])]:0}function
gO(e,d){var
f=du(e,gH,d);function
g(a){var
b=a[2];return[0,eP(a[1]),b]}var
b=a(m[17][68],g,f),c=gK(b);if(c){var
h=c[1],i=function(a,b){return a},j=a(m[17][13],i,b);return[0,C(S[11],f7,y[5],j,h)]}return 0}function
cd(c){if(typeof
c==="number")return[0,l[2],0];else
switch(c[0]){case
0:var
f=c[1],y=[0,[1,af(f)]];return[0,a4(f),y];case
1:return[0,l[2],[1,c[1]]];case
2:var
g=cd(c[1]);return[0,g[1],[2,g[2]]];case
3:var
h=c[1],z=h[2],i=cd(h[1]),j=i[2],k=i[1],m=cd(z),n=m[2],o=m[1],d=a(l[17],k,o),p=a(l[15],k,d),q=a(l[15],o,d),A=a(l[10],p,q),r=a(l[10],d,A),B=a(l[23],r,l[2]);return a(D[2],B,0)?[0,l[2],[3,[0,j,n]]]:[0,r,[3,[0,[5,[0,[0,[1,q]],j]],[5,[0,[0,[1,p]],n]]]]];case
4:return b(e[3],vI);case
5:var
s=c[1],C=s[2],t=cd(s[1]),E=t[2],F=t[1],u=cd(C),G=[5,[0,E,u[2]]];return[0,a(l[10],F,u[1]),G];default:var
v=c[1],w=v[2],x=cd(v[1]),H=[6,[0,x[2],w]];return[0,a(l[19],x[1],w),H]}}function
j1(b){var
a=cd(b);return[0,a[1],a[2]]}function
cK(b){switch(b[0]){case
0:return[0,l[2],[0,b[1]]];case
1:return[0,l[2],[1,b[1]]];case
2:return[0,l[2],[2,b[1]]];case
3:var
d=b[1],t=[3,[1,af(d)]];return[0,a4(d),t];case
4:var
e=b[1],u=[4,[1,af(e)]];return[0,a4(e),u];case
5:var
f=b[1],v=[5,[1,af(f)]];return[0,a4(f),v];case
6:var
g=j1(b[1]),h=g[1],w=[6,g[2]];return[0,a(l[10],h,h),w];case
7:return[0,l[2],[7,b[1]]];case
8:var
x=b[2],i=j1(b[1]),y=i[2],z=i[1],j=cK(x),A=[8,y,j[2]];return[0,a(l[10],z,j[1]),A];case
9:var
B=b[2],k=cK(b[1]),m=k[1],C=k[2],n=cK(B),o=n[1],D=n[2],c=a(l[17],m,o),p=a(l[15],m,c),q=a(l[15],o,c),E=a(l[10],p,q);return[0,a(l[10],c,E),[9,[10,[4,[1,q]],C],[10,[4,[1,p]],D]]];default:var
F=b[2],r=cK(b[1]),G=r[2],H=r[1],s=cK(F),I=[10,G,s[2]];return[0,a(l[10],H,s[1]),I]}}function
bB(a){if(typeof
a==="number")return[0,b(y[5],vJ)];else
switch(a[0]){case
0:return[0,b(y[5],a[1])];case
1:var
c=a[1],i=l5(g(m[15][4],c,1,cn(c)-1|0));return[1,b(y[6],i)];case
2:return[5,bB(a[1])];case
3:var
d=a[1],j=d[1],k=bB(d[2]);return[2,bB(j),k];case
4:var
e=a[1],l=e[1],n=bB(e[2]);return[3,bB(l),n];case
5:var
f=a[1],o=f[1],p=bB(f[2]);return[4,bB(o),p];default:var
h=a[1],q=h[1],r=b(y[3],h[2]);return[6,bB(q),r]}}function
j2(a){var
c=bB(a),d=b(y[5],vK);return Y(b(y[5],vL),d,aM,aT,bR,bt,at,c)}function
gP(a){if(a){var
c=a[2],d=a[1];if(c){var
e=gP(c);return[3,[0,b(y[4],d)],e]}return[0,b(y[4],d)]}return 0}function
j3(c){function
e(c){switch(c[0]){case
0:return[0,b(y[4],c[1])];case
1:return[0,b(y[4],c[1])];case
2:return[0,b(y[4],c[1])];case
6:return[1,j2(c[1])];case
7:return gP(c[1]);case
8:var
h=c[1],i=e(c[2]);return[2,j2(h),i];case
9:var
j=c[1],k=e(c[2]);return[4,e(j),k];case
10:var
l=c[1],m=e(c[2]);return[3,e(l),m];default:var
f=c[1],g=a(d[37],f,vM);return a(D[2],g,0)?0:[5,b(y[5],f)]}}return jV(gH,e(c))}function
bC(a){if(typeof
a==="number")return vN;else
switch(a[0]){case
0:var
j=b(d[52],a[1]);return[0,b(y[2],j)];case
1:var
c=a[1],k=l5(g(m[15][4],c,1,cn(c)-1|0));return[1,b(y[6],k)];case
2:return[5,bC(a[1])];case
3:var
e=a[1],l=e[1],n=bC(e[2]);return[2,bC(l),n];case
4:var
f=a[1],o=f[1],p=bC(f[2]);return[3,bC(o),p];case
5:var
h=a[1],q=h[1],r=bC(h[2]);return[4,bC(q),r];default:var
i=a[1],s=i[1],t=b(y[3],i[2]);return[6,bC(s),t]}}function
j4(a){var
c=bC(a),d=n[6],e=n[7],f=n[8],g=n[5],h=b(y[7],1);return Y(b(y[7],0),h,g,f,e,d,aL,c)}function
j5(c){var
f=cK(c)[2];function
e(k){var
c=k;for(;;)switch(c[0]){case
0:return[0,b(y[4],c[1])];case
1:return[0,b(y[4],c[1])];case
2:return[0,b(y[4],c[1])];case
6:return[1,j4(c[1])];case
7:return gP(c[1]);case
8:var
i=c[2],f=c[1];if(typeof
f==="number")var
g=0;else
if(0===f[0])var
j=a(d[26],f[1],vP),g=1;else
var
g=0;if(!g)var
j=0;if(j){var
c=i;continue}var
n=e(i);return[2,j4(f),n];case
9:var
o=c[1],p=e(c[2]);return[4,e(o),p];case
10:var
q=c[1],r=e(c[2]);return[3,e(q),r];default:var
h=c[1],l=a(d[37],h,vO);if(a(D[2],l,0))return 0;var
m=b(d[52],h);return[5,b(y[2],m)]}}return jV(eO,e(f))}function
vQ(a){var
b=0;function
c(b,c){var
a=gL([0,c[1],c[2]]);if(typeof
a==="number")return b;else
switch(a[0]){case
0:throw[0,dt,a[1]];case
1:return[0,[0,a[1],a[2]],b];default:return[0,[0,a[1],a[2]],b]}}return g(m[17][15],c,b,a)}function
gQ(g,c){var
h=b(l[22],c);if(a(D[2],h,0))return[0,l[2],l[1]];var
d=a(l[14],g,c),i=d[1],e=gQ(c,d[2]),f=e[2],j=e[1],k=a(l[10],i,f);return[0,f,a(l[8],j,k)]}function
j6(n,m,c){return jW(gL,function(o){var
f=o[1],g=m[1],i=f[2],j=f[1],k=g[2],l=g[1],p=o[2],q=m[2],r=f[3],s=g[3];function
h(c,b){var
e=a(S[8],b,p),f=a(S[8],c,q),g=a(S[7],f,e),h=a(d[6],r,b),m=a(d[6],s,c),n=a(d[1],m,h),o=em(k,i),t=ao(b,j);return[0,[0,aW(ao(c,l),t),o,n],g]}var
c=au(n,l),e=au(n,j),C=0===c[0]?0===c[1]?1:0:0;if(!C){var
E=0===e[0]?0===e[1]?1:0:0;if(!E){var
t=b(d[25],e),u=ft(b(d[25],c),t);if(a(D[2],u,-1)){var
v=b(d[15],e);return[0,h(v,b(d[15],c))]}if(0===k){var
w=[0,b(d[25],c)],x=a(d[6],e,w),y=b(d[3],x);return[0,h(y,b(d[15],c))]}if(0===i){var
z=b(d[15],e),A=[0,b(d[25],e)],B=a(d[6],c,A);return[0,h(z,b(d[3],B))]}return 0}}return 0},c)}function
vR(z){var
c=0,b=z;for(;;){if(b){var
n=b[2],e=b[1],o=bv(function(g){return function(f){var
b=f[1],c=g[1];if(0===c[2])if(0===b[2]){var
d=b[1],e=c[1];return i8(function(c,b){var
d=l[2],e=af(b),f=af(c),g=a(l[17],f,e),h=a(l[23],g,d);return a(D[2],h,0)},e,d)}return 0}}(e),n),p=o[1];if(!p){var
c=[0,e,c],b=n;continue}var
q=p[1],x=q[2],y=q[1],f=[0,[0,[0,y,e,x]],a(m[17][10],c,o[2])]}else
var
f=[0,0,c];var
r=f[1],A=f[2];if(r){var
g=r[1],s=g[3],t=s[1],u=g[2],v=u[2],h=u[1],i=g[1],B=s[2],C=i[2],E=i[1],F=af(i[3]),w=gQ(af(C),F),j=[1,w[1]],k=[1,w[2]],G=a(d[6],k,t[3]),H=a(d[6],j,h[3]),I=a(d[1],H,G),J=ao(k,t[1]),K=[0,aW(ao(j,h[1]),J),0,I],L=a(S[8],k,B),M=a(S[8],j,v);return[0,j6(E,[0,K,a(S[7],M,L)],[0,[0,h,v],A])]}return 0}}function
vS(f){var
b=bv(function(c){var
b=c[1];if(0===b[2]){var
e=b[1];return gi(function(c,b){if(!a(d[26],b,vT))if(!a(d[26],b,vU))return 0;return[0,c]},e)}return 0},f),c=b[1],g=b[2];if(c){var
e=c[1];return[0,j6(e[1],e[2],g)]}return 0}function
vV(n){var
c=bv(function(k){var
i=k[1];if(0===i[2]){var
c=i[1];for(;;){var
d=aX(c);if(d){var
b=d[1],e=b[3],j=b[1],f=af(b[2]),g=gi(function(g){return function(d,c){var
b=af(c),e=l[2],f=a(l[17],g,b);return a(l[24],f,e)?[0,[0,d,b]]:0}}(f),e);if(g){var
h=g[1];return[0,[0,[0,j,f],[0,h[1],h[2]]]]}var
c=e;continue}return 0}}return 0},n),e=c[1],o=c[2];if(e){var
f=e[1],g=f[2],h=g[1],i=f[1],j=i[2],k=i[1],p=g[2],q=j[1],r=k[1],m=gQ(k[2],j[2]),s=[1,m[2]],t=[1,m[1]];return[0,jW(gL,function(g){var
c=g[1],e=c[1],i=g[2],j=c[3],k=c[2],l=au(r,e),m=au(q,e),n=a(d[6],m,s),o=a(d[6],l,t),u=a(d[1],o,n),f=b(d[3],u),v=a(S[8],f,p),w=a(S[7],v,i),x=a(d[6],f,h[3]),y=a(d[1],x,j);return[0,[0,[0,aW(ao(f,h[1]),e),k,y],w]]},o)]}return 0}function
gR(a){var
b=[0,vS,[0,vR,[0,vV,0]]];return d8(function(a){return iX(b,a)},a)}function
j7(c){function
e(a){var
c=a[1][1];return ee(function(c,a){return 0!==b(d[25],a)?1:0},c)}return a(m[17][21],e,c)}function
vZ(k,j,h){function
n(F,o){if(j7(o)){var
G=b(m[17][mK],o),H=G[2],c=G[1],J=function(a){return 0===a[2]?1:0},u=a(m[17][30],J,c),v=u[2],w=u[1];if(w)var
L=0,M=function(c,b){function
d(a){return cy(b[1],a[1])}return a(m[17][22],d,w)?c:[0,b[1],c]},x=g(m[17][15],M,L,v);else
var
N=function(a){return a[1]},x=a(m[17][14],N,v);var
O=[0,K,vX],P=function(b,e){var
f=dP(b[2]),k=f?a(d[29],f[1],vW):0;if(k)return b;var
h=bT[1]?jR(e,c):a(eI[2],e,c);if(h){var
i=h[1],g=b[2],j=b[1];return hZ(i,g)?[0,e,i]:[0,j,g]}return b},y=g(m[17][15],P,O,x),z=y[2],A=z[1],Q=y[1];if(A){var
B=z[2];if(B)var
h=[0,[0,A[1],Q,B[1]]],s=1;else
var
s=0}else
var
s=0;if(!s)var
h=0;if(h){var
i=h[1],j=i[3],k=i[2],n=i[1],R=a4(n),T=l[2],U=af(n),V=a(l[8],U,T),W=a4(j),X=af(j),Y=[1,a(l[5],l[2],X)],C=gI([0,[0,ao([1,W],k),1,Y],c]),Z=b(d[3],[1,V]),D=gI([0,[0,ao(b(d[3],[1,R]),k),1,Z],c]);if(C)if(D)var
_=D[1],$=gc(C[1]),aa=b(m[17][6],$),ab=gc(_),f=[0,[0,b(m[17][6],ab),[0,n,k,j],aa]],t=1;else
var
t=0;else
var
t=0;if(!t)var
f=b(e[3],vY)}else
var
f=0;if(f){var
p=f[1],q=p[2],I=q[2],ac=p[3],ad=q[1],ae=p[1],ag=b(d[22],q[3]),r=E(F,I,b(d[24],ad),ag,o);if(typeof
r!=="number"&&0===r[0]){var
ah=r[1],ai=b_(ac),aj=gJ(H),ak=a(S[12],aj,ai),al=b_(ae),am=gJ(H);return[0,[1,F,a(S[12],am,al),I,ak,ah]]}return 0}return 0}throw[0,aV,v0]}function
E(c,j,b,h,g){if(a(d[28],b,h))return v1;var
e=i(c+1|0,[0,[0,[0,j,0,b],[2,c]],g]);if(typeof
e!=="number"&&0===e[0]){var
k=e[1],f=E(c,j,a(d[1],b,v2),h,g);if(typeof
f!=="number"&&0===f[0])return[0,[0,k,f[1]]];return 0}return 0}function
i(c,a){if(j7(a))try{var
d=b(j,a),e=gK(d),f=e?[0,[0,c,e[1],0]]:k?n(c,d):0;return f}catch(a){a=o(a);if(a[1]===dt)return[0,[0,c,a[2],0]];throw a}throw[0,aV,v3]}var
p=0;function
q(d,c){var
f=b(S[2],c[2]);return a(e[6],d,f)}var
f=1+g(m[17][15],q,p,h)|0;try{var
t=i(f,vQ(h)),c=t}catch(a){a=o(a);if(a[1]!==dt)throw a;var
c=[0,[0,f,a[2],0]]}if(typeof
c!=="number"&&0===c[0]){var
r=c[1],s=a(gN[53],0,f-1|0);return[0,a(S[10],s,r)]}return 0}function
v4(k,i,c){function
d(d,c){var
f=0;function
h(d,c){var
f=b(S[2],c[2]);return a(e[6],d,f)}var
i=(1+g(m[17][15],h,f,d)|0)-1|0,j=a(gN[53],0,i);return[0,a(S[10],j,c)]}try{var
f=b(i,c),h=jU(f),j=h?d(f,h[1]):0;return j}catch(a){a=o(a);if(a[1]===dt)return d(c,[0,0,a[2],0]);throw a}}function
gS(d,c,b,a){return bT[1]?v4(d,b,a):vZ(c,b,a)}var
eQ=[0,0];function
j8(i,n,h,f){var
j=i[1],d=g(i[2],n,h,f),l=eQ[1];if(l){var
o=l[1],p=[0,B.caml_sys_getcwd(0)],q=g(bD[14],p,o,v5),c=b(e[49],q),r=du(h,eO,f);a(k[1],c,v6);var
s=a(m[17][68],bM,r),t=b(ai[19],v7);C(k[1],c,v8,t,s);var
u=typeof
d==="number"?0:0===d[0]?(g(k[1],c,v_,j),1):0;if(!u)g(k[1],c,v9,j);b(e[52],c);b(e[65],c)}return d}function
v$(t,s,q){var
u=du(s,eO,q),c=b(av[13],u),e=iT(function(a){return b(ai[8],a[1][1])},c)[1],f=r[1];function
h(b,a){return g(r[4],a[1],a[2],b)}var
i=g(m[17][15],h,f,e),j=r[1];function
k(c,a){var
d=a[1][1];return _(function(c,a,e){var
d=vF(i,b(ai[1][2],a));return d?g(r[4],a,d[1],c):c},c,d)}var
l=g(m[17][15],k,j,c),n=0;function
o(c,b,a){return[0,b,a]}var
p=g(r[13],o,l,n),v=gM(c),w=a(m[18],v,c),d=a(m[18],p,w);function
x(a){var
b=a[1],c=a[2];return[0,eP([0,b[1],b[2]]),c]}var
y=a(m[17][68],x,d);return gS(a(m[17][68],bM,d),t,gR,y)}function
j9(b){function
c(a){var
b=a[1],c=a[2];return[0,eP([0,b[1],b[2]]),c]}return a(m[17][68],c,b)}function
wa(d,g,f){var
c=du(g,eO,f);function
h(a){return b(ai[9],a[1][1])}if(a(m[17][21],h,c)){var
i=j9(c);return gS(a(m[17][68],bM,c),d,gR,i)}var
e=jX(c),j=gM(e),k=j9(jZ(a(m[18],e,j)));return gS(a(m[17][68],bM,c),d,gR,k)}function
j_(c,b,a){return j8([0,wb,v$],c,b,a)}function
j$(c,b,a){return j8([0,wc,wa],c,b,a)}aA(1007,[0,vi,bT,eQ,j3,j5,j_,j$,gO,j0],"Micromega_plugin__Certificate");function
eR(m){var
c=b(bc[25],m),l=[aS,wd,aR(0)],f=[aS,we,aR(0)];function
n(a,c){try{var
d=b(a,0);b(c,0);return d}catch(a){a=o(a);try{b(c,0)}catch(b){throw a}throw a}}function
p(a){try{var
c=[0,b(ea[3],a)];return c}catch(a){a=o(a);if(a===ka)return 0;if(b(cI[18],a))throw l;throw a}}function
q(c,a){var
d=g(O[34],a,0,1);try{g(O[34],a,0,0);var
e=0===c?4:1;g(O[83],a,e,1);var
f=1,b=f}catch(a){a=o(a);if(a[1]!==O[1])throw a;var
b=0}g(O[34],a,d,0);return b}function
r(a){var
c=g(O[34],a,0,1);try{g(O[34],a,0,0);var
b=g(O[83],a,0,1);return b}catch(b){b=o(b);if(b[1]===O[1]){g(O[34],a,c,0);return 0}throw b}}function
h(d,c,a){return q(d,c)?n(a,function(a){return r(c)}):b(a,0)}function
d(i){var
f=g(O[23],i,wf,hK),j=b(O[30],f),d=b(c[1],100);function
n(e){for(;;){var
a=p(j);if(a){var
b=a[1];g(c[10],d,b[1],b[2]);continue}return 0}}try{h(0,f,n);b(e[83],j);var
q=g(O[23],i,wi,hK),r=[0,b(O[31],q),1,d];return r}catch(f){f=o(f);if(f===l){b(e[83],j);var
m=g(O[23],i,wg,hK),k=b(O[31],m);h(1,m,function(h){function
f(b,a){return g(ea[1],k,[0,b,a],wh)}a(c[12],f,d);return b(e[52],k)});return[0,k,1,d]}throw f}}function
j(a,j,i){var
d=a[1],k=a[3];if(0===a[2])throw f;var
l=b(O[33],d);g(c[10],k,j,i);return h(1,l,function(a){g(ea[1],d,[0,j,i],wj);return b(e[52],d)})}function
k(b,d){var
e=b[3];if(0===b[2])throw f;return a(c[7],e,d)}return[0,d,k,j,function(c,e){var
a=[i,function(b){try{var
a=[0,d(c)];return a}catch(a){return 0}}];return function(c){var
d=l6(a),f=mj===d?a[1]:i===d?b(kb[2],a):a;if(f){var
g=f[1];try{var
l=k(g,c);return l}catch(a){a=o(a);if(a===L){var
h=b(e,c);j(g,c,h);return h}throw a}}return b(e,c)}}]}aA(1010,[0,eR],"Micromega_plugin__Persistent_cache");function
h(a){var
c=l6(a);return mj===c?a[1]:i===c?b(kb[2],a):a}var
gT=e[8],kc=[0,gT],gU=[0,1],kd=[0,gT];function
ke(a){return[0,bT[1],gU[1],kd[1]]}function
gV(a){return kc[1]}function
kf(b,a){function
c(b){var
c=b?b[1]:gT;a[1]=c;return 0}function
d(b){return[0,a[1]]}return[0,0,g(f[21],e[17],b,wk),b,d,c]}function
wl(a){gU[1]=a;return 0}var
wo=[0,0,wn,wm,function(a){return gU[1]},wl];function
wp(a){bT[1]=a;return 0}var
ws=[0,0,wr,wq,function(a){return bT[1]},wp];function
wt(a){eQ[1]=a;return 0}var
ww=[0,0,wv,wu,function(a){return eQ[1]},wt];a(dv[4],0,ws);a(dv[6],0,ww);var
wy=kf(wx,kc);a(dv[3],0,wy);var
wA=kf(wz,kd);a(dv[3],0,wA);a(dv[4],0,wo);var
wC=a(e[26],dw[20],kg),wD=a(e[26],dw[19],wC),wE=a(e[26],[0,wB,0],wD),wF=a(e[26],dw[21],wE);function
V(d,c,a){var
e=g(dw[18],d,c,a),f=b(wJ[23],e);return b(j[9],f)}var
wK=dw[21];function
aw(a){return V(wL,wK,a)}function
t(a){return V(wM,wF,a)}function
bE(a){return V(wN,wG,a)}function
aO(a){return V(wO,wH,a)}function
bF(a){return V(wP,wI,a)}function
a6(a){return V(wQ,kg,a)}var
gW=[i,function(a){return aw(wR)}],gX=[i,function(a){return aw(wS)}],kh=[i,function(a){return aw(wT)}],wV=[i,function(a){return aw(wU)}],ki=[i,function(a){return aw(wW)}],gY=[i,function(a){return aw(wX)}],wZ=[i,function(a){return t(wY)}],w1=[i,function(a){return t(w0)}],kj=[i,function(a){return t(w2)}],w4=[i,function(a){return aw(w3)}],w6=[i,function(a){return aw(w5)}],kk=[i,function(a){return aw(w7)}],gZ=[i,function(a){return aw(w8)}],w_=[i,function(a){return aw(w9)}],xa=[i,function(a){return aw(w$)}],xc=[i,function(a){return aw(xb)}],xe=[i,function(a){return aw(xd)}],xg=[i,function(a){return bE(xf)}],xi=[i,function(a){return bE(xh)}],xk=[i,function(a){return bE(xj)}],xm=[i,function(a){return bE(xl)}],xo=[i,function(a){return bE(xn)}],aP=[i,function(a){return bE(xp)}],xr=[i,function(a){return bE(xq)}],xt=[i,function(a){return bE(xs)}],xv=[i,function(a){return bE(xu)}],dx=[i,function(a){return t(xw)}],eS=[i,function(a){return t(xx)}],kl=[i,function(a){return t(xy)}],km=[i,function(a){return t(xz)}],xB=[i,function(a){return a6(xA)}],xD=[i,function(a){return a6(xC)}],xF=[i,function(a){return a6(xE)}],xH=[i,function(a){return a6(xG)}],xJ=[i,function(a){return a6(xI)}],xL=[i,function(a){return a6(xK)}],xN=[i,function(a){return a6(xM)}],xP=[i,function(a){return a6(xO)}],xR=[i,function(a){return a6(xQ)}],xT=[i,function(a){return a6(xS)}],kn=[i,function(a){return t(xU)}],ko=[i,function(a){return t(xV)}],kp=[i,function(a){return t(xW)}],xY=[i,function(a){return t(xX)}],x0=[i,function(a){return t(xZ)}],x2=[i,function(a){return t(x1)}],x4=[i,function(a){return t(x3)}],x6=[i,function(a){return bF(x5)}],x8=[i,function(a){return bF(x7)}],x_=[i,function(a){return bF(x9)}],ya=[i,function(a){return bF(x$)}],g0=[i,function(a){return aw(yb)}],kq=[i,function(a){return bF(yc)}],kr=[i,function(a){return bF(yd)}],ks=[i,function(a){return bF(ye)}],kt=[i,function(a){return bF(yf)}],ku=[i,function(a){return bF(yg)}],yi=[i,function(a){return t(yh)}],yk=[i,function(a){return t(yj)}],ym=[i,function(a){return t(yl)}],kv=[i,function(a){return t(yn)}],kw=[i,function(a){return t(yo)}],kx=[i,function(a){return t(yp)}],ky=[i,function(a){return t(yq)}],kz=[i,function(a){return t(yr)}],yt=[i,function(a){return aO(ys)}],yv=[i,function(a){return aO(yu)}],yx=[i,function(a){return aO(yw)}],yz=[i,function(a){return aO(yy)}],eT=[i,function(a){return aO(yA)}],eU=[i,function(a){return aO(yB)}],g1=[i,function(a){return aO(yC)}],eV=[i,function(a){return aO(yD)}],kA=[i,function(a){return aO(yE)}],eW=[i,function(a){return aO(yF)}],yH=[i,function(a){return aO(yG)}],kB=[i,function(a){return aO(yI)}],kC=[i,function(a){return aO(yJ)}],yL=[i,function(a){return t(yK)}],yN=[i,function(a){return t(yM)}],yP=[i,function(a){return t(yO)}],yR=[i,function(a){return t(yQ)}],yT=[i,function(a){return t(yS)}],yV=[i,function(a){return t(yU)}],yX=[i,function(a){return t(yW)}],yZ=[i,function(a){return t(yY)}],y1=[i,function(a){return t(y0)}],y3=[i,function(a){return t(y2)}],y5=[i,function(a){return t(y4)}],y7=[i,function(a){return t(y6)}],y9=[i,function(a){return t(y8)}],y$=[i,function(a){return t(y_)}],zb=[i,function(a){return t(za)}],zd=[i,function(a){return t(zc)}],zf=[i,function(a){return t(ze)}],zh=[i,function(a){return t(zg)}],zj=[i,function(a){return t(zi)}],zl=[i,function(a){return t(zk)}],zn=[i,function(a){return t(zm)}],zp=[i,function(a){return t(zo)}],zr=[i,function(a){return t(zq)}],zt=[i,function(a){return a6(zs)}],zx=[i,function(a){return V(zw,zv,zu)}],zB=[i,function(a){return V(zA,zz,zy)}],zF=[i,function(a){return V(zE,zD,zC)}],zJ=[i,function(a){return V(zI,zH,zG)}],zN=[i,function(a){return V(zM,zL,zK)}],zR=[i,function(a){return V(zQ,zP,zO)}],zV=[i,function(a){return V(zU,zT,zS)}],zZ=[i,function(a){return V(zY,zX,zW)}],kD=[i,function(a){return V(z2,z1,z0)}],g2=[i,function(a){return V(z5,z4,z3)}],z9=[i,function(a){return V(z8,z7,z6)}],kE=[i,function(a){return V(Aa,z$,z_)}],M=[aS,Ab,aR(0)];function
g3(c,e){var
b=a(j[3],c,e);switch(b[0]){case
9:var
f=b[2],d=a(j[3],c,b[1]);if(12===d[0])return[0,d[1][1][2],f];throw M;case
12:return[0,b[1][1][2],[0]];default:throw M}}function
g4(a,d){var
b=g3(a,d),c=b[1],e=b[2];if(1===c)return 0;if(2===c)return[0,g4(a,H(e,0)[1])];throw M}function
Ac(c,a){var
d=b(ag[5],a);return g(k[1],c,Ad,d)}function
dy(a){if(a){var
c=[0,dy(a[1])],d=[0,h(w6),c];return b(j[23],d)}return h(w4)}function
dz(a,e){var
b=g3(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw M;switch(d){case
0:return[0,dz(a,H(c,0)[1])];case
1:return[1,dz(a,H(c,0)[1])];default:return 0}}function
bG(a){if(typeof
a==="number")return h(xk);else{if(0===a[0]){var
c=[0,bG(a[1])],d=[0,h(xo),c];return b(j[23],d)}var
e=[0,bG(a[1])],f=[0,h(xm),e];return b(j[23],f)}}function
g5(c,a){var
d=b(ag[3],a);return g(k[1],c,Ae,d)}function
kF(e,d,c){switch(a(j[3],d,c)[0]){case
10:case
12:var
f=T(kG[2],0,0,e,d,c);try{var
g=[0,h(zt),[0,f,c]],i=b(j[23],g);C(Af[24],0,e,d,i);var
k=1;return k}catch(a){a=o(a);if(a===L)return 0;throw a}default:return 0}}function
g6(c,b,e){var
d=a(j[3],b,e);switch(d[0]){case
9:var
g=d[2],f=kF(c,b,d[1]);if(f){var
h=function(a){return g6(c,b,a)};return a(kH[21],h,g)}return f;case
10:case
12:return kF(c,b,e);default:return 0}}function
kI(a,e){var
b=g3(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw M;switch(d){case
0:return 0;case
1:return[0,dz(a,H(c,0)[1])];default:return[1,dz(a,H(c,0)[1])]}}function
ap(a){if(typeof
a==="number")return h(xr);else{if(0===a[0]){var
c=[0,bG(a[1])],d=[0,h(xt),c];return b(j[23],d)}var
e=[0,bG(a[1])],f=[0,h(xv),e];return b(j[23],f)}}function
bV(c,a){var
d=b(ag[1],a),e=b(l[33],d);return g(k[1],c,Ag,e)}function
ce(a){var
c=bG(a[2]),d=[0,ap(a[1]),c],e=[0,h(kl),d];return b(j[23],e)}function
Ah(b,e){var
c=a(j[3],b,e);if(9===c[0]){var
d=c[2],f=c[1],i=h(kl);if(g(j[ad],b,f,i)){var
k=dz(b,H(d,1)[2]);return[0,kI(b,H(d,0)[1]),k]}throw M}throw M}function
bg(a){if(typeof
a==="number")return 0===a?h(xB):h(xD);else
switch(a[0]){case
0:var
e=[0,ce(a[1])],f=[0,h(xF),e];return b(j[23],f);case
1:var
g=[0,ap(a[1])],i=[0,h(xH),g];return b(j[23],i);case
2:var
k=a[1],l=bg(a[2]),m=[0,bg(k),l],n=[0,h(xJ),m];return b(j[23],n);case
3:var
o=a[1],p=bg(a[2]),q=[0,bg(o),p],r=[0,h(xL),q];return b(j[23],r);case
4:var
s=a[1],t=bg(a[2]),u=[0,bg(s),t],v=[0,h(xN),u];return b(j[23],v);case
5:var
c=a[2],w=a[1];if(0===c[0])var
x=ap(c[1]),y=h(kk),z=[0,h(aP),y,x],A=[0,h(xc),z],d=b(j[23],A);else
var
D=dy(c[1]),E=h(kk),F=[0,h(aP),E,D],G=[0,h(xe),F],d=b(j[23],G);var
B=[0,bg(w),d],C=[0,h(xP),B];return b(j[23],C);case
6:var
H=[0,bg(a[1])],I=[0,h(xR),H];return b(j[23],I);default:var
J=[0,bg(a[1])],K=[0,h(xT),J];return b(j[23],K)}}function
g7(c,d,a){if(a){var
e=a[1],f=g7(c,d,a[2]),g=[0,c,b(d,e),f],i=[0,h(wZ),g];return b(j[23],i)}var
k=[0,h(w1),[0,c]];return b(j[23],k)}function
kJ(d,k,a){function
c(a){switch(a[0]){case
0:var
l=[0,d,b(k,a[1])],m=[0,h(yN),l];return b(j[23],m);case
1:var
n=[0,d,bG(a[1])],o=[0,h(yL),n];return b(j[23],o);case
2:var
p=a[1],q=c(a[2]),r=[0,d,c(p),q],s=[0,h(yP),r];return b(j[23],s);case
3:var
t=a[1],u=c(a[2]),v=[0,d,c(t),u],w=[0,h(yV),v];return b(j[23],w);case
4:var
x=a[1],y=c(a[2]),z=[0,d,c(x),y],A=[0,h(yT),z];return b(j[23],A);case
5:var
B=[0,d,c(a[1])],C=[0,h(yR),B];return b(j[23],C);default:var
e=a[2],D=a[1];if(e)var
g=[0,bG(e[1])],i=[0,h(xi),g],f=b(j[23],i);else
var
f=h(xg);var
E=[0,d,c(D),f],F=[0,h(yX),E];return b(j[23],F)}}return c(a)}function
kK(d,e,a){function
c(a){switch(a[0]){case
0:var
f=[0,d,b(e,a[1])],g=[0,h(y1),f];return b(j[23],g);case
1:var
i=a[1],k=c(a[2]),l=[0,d,bG(i),k],m=[0,h(y3),l];return b(j[23],m);default:var
n=a[2],o=a[1],p=c(a[3]),q=bG(n),r=[0,d,c(o),q,p],s=[0,h(yZ),r];return b(j[23],s)}}return c(a)}function
bh(d,c,a){function
b(c,a){switch(a[0]){case
0:return C(k[1],c,Al,d,a[1]);case
1:return W(k[1],c,Am,g5,a[1],b,a[2]);default:return l7(k[1],c,An,b,a[1],g5,a[2],b,a[3])}}return b(c,a)}function
cL(f,e,a){var
c=h(f);function
d(a){if(typeof
a==="number"){var
f=[0,h(zr),[0,c]];return b(j[23],f)}else
switch(a[0]){case
0:var
g=[0,c,dy(a[1])],i=[0,h(zf),g];return b(j[23],i);case
1:var
k=[0,c,kK(c,e,a[1])],l=[0,h(zh),k];return b(j[23],l);case
2:var
m=a[1],n=d(a[2]),o=[0,c,kK(c,e,m),n],p=[0,h(zl),o];return b(j[23],p);case
3:var
q=a[1],r=d(a[2]),s=[0,c,d(q),r],t=[0,h(zj),s];return b(j[23],t);case
4:var
u=a[1],v=d(a[2]),w=[0,c,d(u),v],x=[0,h(zn),w];return b(j[23],x);default:var
y=[0,c,b(e,a[1])],z=[0,h(zp),y];return b(j[23],z)}}return d(a)}function
bH(e,c,b){function
d(c,b){if(typeof
b==="number")return a(k[1],c,Ao);else
switch(b[0]){case
0:return C(k[1],c,Ap,Ac,b[1]);case
1:var
f=b[1],g=function(a,b){return bh(e,a,b)};return C(k[1],c,Aq,g,f);case
2:var
h=b[2],i=b[1],j=function(a,b){return bh(e,a,b)};return W(k[1],c,Ar,j,i,d,h);case
3:return W(k[1],c,As,d,b[1],d,b[2]);case
4:return W(k[1],c,At,d,b[1],d,b[2]);default:return C(k[1],c,Au,e,b[1])}}return d(c,b)}function
dA(d,e,c){var
f=c[2],g=c[1],i=kJ(d,e,c[3]);switch(f){case
0:var
a=h(y5);break;case
1:var
a=h(y7);break;case
2:var
a=h(y9);break;case
3:var
a=h(zb);break;case
4:var
a=h(y$);break;default:var
a=h(zd)}var
k=[0,d,kJ(d,e,g),a,i],l=[0,h(z9),k];return b(j[23],l)}function
eX(d,c,b){try{var
e=function(a){var
b=h(a[1]);return g(j[ad],d,c,b)},i=a(f[33],e,b)[2];return i}catch(a){a=o(a);if(a===L)throw M;throw a}}var
kL=[0,[0,x6,5],[0,[0,x8,3],[0,[0,ya,4],[0,[0,x_,2],0]]]],kM=[0,[0,yt,5],[0,[0,yv,3],[0,[0,yz,4],[0,[0,yx,2],0]]]],kN=[0,[0,yk,4],[0,[0,yi,2],[0,[0,ym,0],0]]];function
kO(a,c,b){return T(Av[81],0,a[1],a[2],c,b)}function
Aw(c,b){var
a=b[2],d=b[1],e=c[2],f=a.length-1;if(2===f){var
i=a[1],k=a[2];return[0,eX(e,d,kL),i,k]}if(3===f){var
l=a[1],m=h(g0);if(g(j[ad],e,d,m))if(kO(c,l,h(aP))){var
n=H(a,2)[3];return[0,0,H(a,1)[2],n]}throw M}throw M}function
Ax(c,b){var
a=b[2],d=b[1],e=c[2],f=a.length-1;if(2===f){var
i=a[1],k=a[2];return[0,eX(e,d,kM),i,k]}if(3===f){var
l=a[1],m=a[2],n=a[3],o=h(g0);if(g(j[ad],e,d,o))if(kO(c,l,h(eS)))return[0,0,m,n];throw M}throw M}function
Ay(c,b){var
a=b[2],d=b[1];if(2===a.length-1){var
e=H(a,1)[2],f=H(a,0)[1];return[0,eX(c[2],d,kN),f,e]}throw M}function
eY(a){return[0,0,a]}function
AA(c,h,f){var
d=c[2],e=C(j[105],c[1],d,h,f);if(e){var
i=e[1],k=b(kP[152],d),l=g(AB[4],0,k,i);try{var
m=a(kP[37],d,l)}catch(a){a=o(a);if(a[1]===AC[26])return 0;throw a}return[0,[0,c[1],m]]}return 0}function
eZ(c,d){function
f(d,a,c,b){if(a){var
g=a[1],i=a[2],h=AA(d,g,b);if(h)return[0,h[1],a,c];var
e=f(d,i,c+1|0,b);return[0,e[1],[0,g,e[2]],e[3]]}return[0,d,[0,b,0],c]}var
a=f(c[2],c[1],1,d),e=a[2],g=a[1];return[0,[0,e,g],b(y[1],a[3])]}function
kQ(c,d){var
a=c[1],b=1,e=c[2][2];for(;;){if(a){var
f=a[2];if(g(j[ad],e,a[1],d))return b;var
a=f,b=b+1|0;continue}throw[0,jN,AD]}}function
g8(k,l,x,w,c,b){function
n(c,b){var
a=eZ(c,b);return[0,[1,a[2]],a[1]]}function
d(b,c){function
y(g,f,b){var
h=b[2],c=d(g,b[1]),i=c[1],e=d(c[2],h),j=e[2];return[0,a(f,i,e[1]),j]}try{var
F=[0,[0,a(l,k,c)],b];return F}catch(l){l=o(l);if(l===M){var
m=a(j[3],k[2],c);if(9===m[0]){var
e=m[2],p=m[1];if(10===a(j[3],k[2],p)[0]){var
z=k[2];try{var
u=function(a){var
b=h(a[1]);return g(j[ad],z,p,b)},v=a(f[33],u,w)[2],i=v}catch(a){a=o(a);if(a!==L)throw a;var
i=Az}if(typeof
i==="number"){if(0===i){var
q=d(b,H(e,0)[1]);return[0,[5,q[1]],q[2]]}try{var
s=d(b,H(e,0)[1]),A=s[2],B=s[1],C=[0,a(x,B,H(e,1)[2]),A];return C}catch(a){a=o(a);if(a===M){var
r=eZ(b,c);return[0,[1,r[2]],r[1]]}throw a}}else{if(0===i[0]){var
D=i[1],E=H(e,1)[2];return y(b,D,[0,H(e,0)[1],E])}var
t=eZ(b,c);return[0,[1,t[2]],t[1]]}}return n(b,c)}return n(b,c)}throw l}}return d(c,b)}var
AE=[0,[0,ks,0],[0,[0,ku,1],0]],AF=[0,[0,kt,[0,function(b,a){return[4,b,a]}]],AE],AG=[0,[0,kr,[0,function(b,a){return[3,b,a]}]],AF],AH=[0,[0,kq,[0,function(b,a){return[2,b,a]}]],AG],AI=[0,[0,kx,0],[0,[0,kz,1],0]],AJ=[0,[0,ky,[0,function(b,a){return[4,b,a]}]],AI],AK=[0,[0,kw,[0,function(b,a){return[3,b,a]}]],AJ],AL=[0,[0,kv,[0,function(b,a){return[2,b,a]}]],AK],AM=[0,[0,g1,0],[0,[0,eW,1],0]],AN=[0,[0,eV,[0,function(b,a){return[4,b,a]}]],AM],AO=[0,[0,eU,[0,function(b,a){return[3,b,a]}]],AN],AP=[0,[0,eT,[0,function(b,a){return[2,b,a]}]],AO];function
g9(d,c,b){return a(d,c[2],b)}function
kR(e,b,d){try{var
c=a(e,b,d);return c}catch(c){c=o(c);if(c===M){if(g6(b[1],b[2],d))return a(e,b,g(AQ[6],b[1],b[2],d));throw M}throw c}}function
g_(a,b){return g9(kI,a,b)}function
kS(a,b){return g9(Ah,a,b)}function
AR(a,b){return g9(g4,a,b)}function
kT(c){function
d(g,f){var
h=eY(c),e=bb(a(kT(c),h,f)[1]);if(e){var
d=e[1];if(typeof
d!=="number"&&1===d[0])return AS;return[6,g,b(n[17],d)]}throw M}return function(a,b){return g8(c,g_,d,AH,a,b)}}var
AT=0,AU=[0,[0,eV,function(b,a){return[4,b,a]}],AT],AV=[0,[0,eU,function(b,a){return[3,b,a]}],AU],AW=[0,[0,eT,function(b,a){return[2,b,a]}],AV];function
AY(f,c){var
b=f[2];function
d(i){var
k=a(j[3],b,i);switch(k[0]){case
9:var
c=k[2],e=k[1];try{var
s=eX(b,e,AW),t=d(H(c,0)[1]),u=a(s,t,d(H(c,1)[2]));return u}catch(a){a=o(a);if(a===M){var
m=h(kA);if(g(j[ad],b,e,m)){var
l=d(H(c,0)[1]);if(at(aD(l),AX))throw M;return[6,l]}var
n=h(eW);if(g(j[ad],b,e,n)){var
p=[1,kR(AR,f,H(c,1)[2])];return[5,d(H(c,0)[1]),p]}var
q=h(kC);if(g(j[ad],b,e,q))return[0,kS(f,H(c,0)[1])];var
r=h(kB);if(g(j[ad],b,e,r))return[1,kR(g_,f,H(c,0)[1])];throw M}throw a}case
10:var
v=h(kn);if(g(j[ad],b,i,v))return 0;var
w=h(ko);if(g(j[ad],b,i,w))return 1;throw M;default:throw M}}return d(c)}function
AZ(d){function
a(c,e){var
a=g_(d,e);if(typeof
a!=="number"&&1===a[0]){if(0===c[0])return[0,fT(c[1],a)];throw M}return[6,c,b(n[17],a)]}return function(b,c){return g8(d,kS,a,AL,b,c)}}function
A0(a){function
c(d,c){var
e=g4(a[2],c);return[6,d,b(h6[1],e)]}return function(b,d){return g8(a,AY,c,AP,b,d)}}function
g$(n,h,m,l,c){var
d=a(j[3],c[2],l);if(9===d[0]){var
f=a(n,c,[0,d[1],d[2]]),o=f[3],p=f[1],i=g(h,c,m,f[2]),q=i[1],k=g(h,c,i[2],o);return[0,[0,q,p,k[1]],k[2]]}return b(e[3],A1)}function
e0(a,b,c){return g$(Aw,kT,a,b,c)}function
e1(a,b,c){return g$(Ay,AZ,a,b,c)}function
A2(a,b,c){return g$(Ax,A0,a,b,c)}function
A3(b,a){return[2,b,a]}function
A4(b,a){return[3,b,a]}function
A5(b,a){return[2,[5,b,0,a],[5,a,0,b]]}function
A6(b,a){return[5,b,0,a]}function
e2(e,d,c,b){if(typeof
c!=="number"&&0===c[0])if(typeof
b!=="number"&&0===b[0])return[0,d];return a(e,c,b)}function
kU(l,k,f,e,d){var
i=l[2];function
B(e,d,c){try{var
a=g(k,e,c,l),f=a[2],h=a[1],i=[0,[1,h,[0,d,c]],f,b(bw[2],d)];return i}catch(a){a=o(a);if(b(cI[18],a))return[0,[0,c],e,d];throw a}}function
c(f,e,d){var
k=a(j[3],i,d);switch(k[0]){case
6:var
z=k[3],G=k[2],Z=0===k[1][1]?0:g(j[fw][13],i,1,z)?0:1;if(!Z){var
o=c(f,e,G),H=o[1],p=c(o[2],o[3],z),I=p[3],J=p[2];return[0,e2(A6,d,H,p[1]),J,I]}break;case
9:var
m=k[2],n=k[1],A=m.length-1;if(!(3<=A))switch(A){case
0:break;case
1:var
K=m[1],L=h(kh);if(g(j[ad],i,n,L)){var
q=c(f,e,K);return[0,[4,q[1]],q[2],q[3]]}break;default:var
r=m[1],s=m[2],N=h(gW);if(g(j[ad],i,n,N)){var
t=c(f,e,r),O=t[1],u=c(t[2],t[3],s),P=u[3],Q=u[2];return[0,e2(A3,d,O,u[1]),Q,P]}var
R=h(gX);if(g(j[ad],i,n,R)){var
v=c(f,e,r),S=v[1],w=c(v[2],v[3],s),T=w[3],U=w[2];return[0,e2(A4,d,S,w[1]),U,T]}var
V=h(wV);if(g(j[ad],i,n,V)){var
x=c(f,e,r),W=x[1],y=c(x[2],x[3],s),X=y[3],Y=y[2];return[0,e2(A5,d,W,y[1]),Y,X]}}return B(f,e,d)}var
E=h(ki);if(g(j[ad],i,d,E))return[0,0,f,e];var
F=h(gY);if(g(j[ad],i,d,F))return[0,1,f,e];var
D=C(kG[3],0,l[1],l[2],d);if(b(A7[10],D))return[0,[0,d],f,e];throw M}return c(f,e,d)}function
kV(f,e,a){function
c(c,a){var
d=[0,h(gZ),a],e=[0,h(gZ),d],g=b(kH[12],[0,f,[0,j[16],e]]),i=[0,h(c),g];return b(j[23],i)}function
d(a){if(typeof
a==="number")return 0===a?c(zx,0):c(zB,0);else
switch(a[0]){case
0:return c(zV,[0,a[1],0]);case
1:var
f=a[1],g=[0,h(xa),0];return c(zR,[0,b(e,f),g]);case
2:var
i=a[1],k=[0,d(a[2]),0];return c(zF,[0,d(i),k]);case
3:var
l=a[1],m=[0,d(a[2]),0];return c(zJ,[0,d(l),m]);case
4:return c(zN,[0,d(a[1]),0]);default:var
n=a[1],o=[0,d(a[3]),0],p=[0,h(gZ)],q=[0,h(w_),p],r=[0,b(j[23],q),o];return c(zZ,[0,d(n),r])}}return d(a)}function
ha(b,a){function
d(h,g){var
b=h,a=g;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
0:return eZ(b,a[1])[1];case
4:var
a=a[1];continue;case
5:var
f=a[3],e=a[1],c=1;break;case
1:var
c=0;break;default:var
f=a[2],e=a[1],c=1}if(c){var
b=d(b,e),a=f;continue}return b}}return d(eY(b),a)}var
e3=[i,function(m){function
c(a){var
b=a[2];return[0,b,h(a[1])]}var
d=a(f[17],c,kL);function
e(a){var
c=b(ag[4],a);return ap(b(y[7],c))}var
g=h(ku),i=h(kt),j=h(ks),k=h(kr),l=h(kq);return[0,h(aP),ap,l,k,j,i,g,e,d]}],e4=[i,function(m){function
c(a){var
b=a[2];return[0,b,h(a[1])]}var
d=a(f[17],c,kN);function
e(a){var
c=b(ag[4],a);return ap(b(y[7],c))}var
g=h(kz),i=h(ky),j=h(kx),k=h(kw),l=h(kv);return[0,h(dx),ce,l,k,j,i,g,e,d]}];function
a7(a){if(typeof
a==="number")return 0===a?h(kn):h(ko);else
switch(a[0]){case
0:var
e=[0,ce(a[1])],f=[0,h(kC),e];return b(j[23],f);case
1:var
g=[0,ap(a[1])],i=[0,h(kB),g];return b(j[23],i);case
2:var
k=a[1],l=a7(a[2]),m=[0,a7(k),l],n=[0,h(eT),m];return b(j[23],n);case
3:var
o=a[1],p=a7(a[2]),q=[0,a7(o),p],r=[0,h(eU),q];return b(j[23],r);case
4:var
s=a[1],t=a7(a[2]),u=[0,a7(s),t],v=[0,h(eV),u];return b(j[23],v);case
5:var
c=a[2],d=a[1];if(0===c[0]){var
w=ap(c[1]),x=[0,a7(d),w],y=[0,h(yH),x];return b(j[23],y)}var
z=dy(c[1]),A=[0,a7(d),z],B=[0,h(eW),A];return b(j[23],B);case
6:var
C=[0,a7(a[1])],D=[0,h(kA),C];return b(j[23],D);default:var
E=[0,a7(a[1])],F=[0,h(g1),E];return b(j[23],F)}}var
A8=[i,function(m){function
c(a){var
b=a[2];return[0,b,h(a[1])]}var
d=a(f[17],c,kM);function
e(a){var
c=b(ag[4],a);return dy(b(y[4],c))}var
g=h(eW),i=h(eV),j=h(g1),k=h(eU),l=h(eT);return[0,h(eS),a7,l,k,j,i,g,e,d]}];function
kW(i,h,g){var
c=[0,i,h,g];for(;;){var
e=c[1];if(0===e)return c[3];var
d=c[2];if(d){var
f=d[1],k=c[3],l=d[2],m=f[2],n=[0,a(kX[4],f[1],0),m,k],c=[0,e-1|0,l,b(j[20],n)];continue}throw[0,aV,A9]}}function
kY(u,d,c){function
i(d){var
c=d;for(;;)switch(c[0]){case
0:return s[1];case
1:var
e=b(ag[3],c[1]);return b(s[5],e);case
5:case
6:var
c=c[1];continue;default:var
f=c[1],g=i(c[2]),h=i(f);return a(s[7],h,g)}}function
m(k){var
b=k;for(;;){if(typeof
b==="number")var
c=0;else
switch(b[0]){case
1:var
d=b[1],g=d[1],h=i(d[3]),j=i(g);return a(s[7],j,h);case
4:var
b=b[1];continue;case
5:var
f=b[3],e=b[1],c=1;break;case
0:var
c=0;break;default:var
f=b[2],e=b[1],c=1}if(c){var
l=m(f),n=m(e);return a(s[7],n,l)}return s[1]}}var
t=m(c),v=b(s[21],t);function
w(b,a){return[0,a,b+1|0]}var
n=a(f[18],w,v),p=ha(u,c);function
x(c){var
e=d[1],f=a(k[4],A_,c[2]);return[0,b(bU[1][6],f),e]}var
l=a(f[17],x,n),y=p[1];function
z(c,f){var
d=j[16],e=a(k[4],A$,c+1|0);return[0,b(bU[1][6],e),d]}var
q=a(f[18],z,y);function
A(b,a){return[0,a[1],b[1]]}var
B=g(f[23],A,n,l);function
r(g,c){function
e(c){switch(c[0]){case
0:return b(d[2],c[1]);case
1:var
h=b(ag[3],c[1]),i=g+a(f[38],h,n)|0;return b(j[10],i);case
2:var
k=c[1],l=e(c[2]),m=[0,e(k),l];return b(j[23],[0,d[3],m]);case
3:var
o=c[1],p=e(c[2]),q=[0,e(o),p];return b(j[23],[0,d[4],q]);case
4:var
r=c[1],s=e(c[2]),t=[0,e(r),s];return b(j[23],[0,d[6],t]);case
5:var
u=[0,e(c[1])];return b(j[23],[0,d[5],u]);default:var
v=c[1],w=b(d[8],c[2]),x=[0,e(v),w];return b(j[23],[0,d[7],x])}}return e(c)}function
C(g,e,c){try{var
l=[0,a(f[38],g,d[9]),[0,e,c]],m=b(j[23],l);return m}catch(a){a=o(a);if(a===L){var
i=[0,d[1],e,c],k=[0,h(g0),i];return b(j[23],k)}throw a}}function
e(d,c,a){if(typeof
a==="number")return 0===a?h(ki):h(gY);else
switch(a[0]){case
0:var
m=d+kQ(p,a[1])|0;return b(j[10],m);case
1:var
f=a[1],i=f[2],k=f[1],l=r(c,f[3]);return C(i,r(c,k),l);case
2:var
n=a[1],o=e(d,c,a[2]),q=[0,e(d,c,n),o],s=[0,h(gW),q];return b(j[23],s);case
3:var
t=a[1],u=e(d,c,a[2]),v=[0,e(d,c,t),u],w=[0,h(gX),v];return b(j[23],w);case
4:var
x=a[1],y=h(gY),z=e(d,c,x);return g(j[35],z,0,y);default:var
A=a[1],B=e(d+1|0,c+1|0,a[3]),D=e(d,c,A);return g(j[35],D,0,B)}}var
D=b(f[1],l),E=b(f[1],q),F=br(function(c){var
d=kQ(p,c),e=a(k[4],Ba,d),f=b(bU[1][6],e);return b(j[11],f)},c),G=b(f[9],B),H=b(f[9],q),I=e(b(f[1],l),0,c);function
J(a){return[0,[0,a[1]],a[2]]}var
K=kW(D,a(f[17],J,l),I);function
M(a){return[0,[0,a[1]],a[2]]}return[0,kW(E,a(f[17],M,q),K),H,G,F]}function
kZ(g,f){var
d=f,c=g;for(;;){if(c){var
e=c[1],h=c[2],i=e[3],k=e[2],l=b(bU[1][6],e[1]),m=a(kX[4],l,0),d=C(j[47],m,k,i,d),c=h;continue}return d}}var
Be=[i,function(a){return V(Bd,Bc,Bb)}],Bi=[i,function(a){return V(Bh,Bg,Bf)}],Bm=[i,function(a){return V(Bl,Bk,Bj)}],Bq=[i,function(a){return V(Bp,Bo,Bn)}];function
e5(c,a){if(typeof
a==="number"){var
d=[0,h(Bm),[0,c]];return b(j[23],d)}else{if(0===a[0]){var
e=[0,c,a[1]],f=[0,h(Bi),e];return b(j[23],f)}var
g=a[2],i=a[1],k=e5(c,a[3]),l=[0,c,e5(c,i),g,k],m=[0,h(Be),l];return b(j[23],m)}}function
k0(a){if(a){var
c=a[1][1],d=0,e=function(d,a){var
e=a[1];return d1(c,b(y[1],a[2]),e,d)};return g(f[20],e,d,a)}return 0}function
e6(a){if(typeof
a==="number")return h(xY);else
switch(a[0]){case
0:var
c=a[1],d=e6(a[2]),e=[0,cL(aP,ap,c),d],f=[0,h(x0),e];return b(j[23],f);case
1:var
g=a[1],i=e6(a[2]),k=[0,cL(aP,ap,g),i],l=[0,h(x2),k];return b(j[23],l);default:var
m=a[3],n=a[2],o=a[1],p=g7(h(kp),e6,m),q=cL(aP,ap,n),r=[0,cL(aP,ap,o),q,p],s=[0,h(x4),r];return b(j[23],s)}}function
k1(a){return e6(a)}function
bj(b,a){return W(k[1],b,Br,bV,a[1],g5,a[2])}function
cf(c,b){if(typeof
b==="number")return a(k[1],c,Bs);else
switch(b[0]){case
0:var
d=b[2],e=b[1],f=function(a,b){return bH(bV,a,b)};return W(k[1],c,Bt,f,e,cf,d);case
1:var
g=b[2],h=b[1],i=function(a,b){return bH(bV,a,b)};return W(k[1],c,Bu,i,h,cf,g);default:var
j=b[3],l=b[2],m=b[1],n=function(a,c){function
b(c,a){if(a){var
d=a[2],e=a[1];return d?W(k[1],c,Ai,cf,e,b,d):C(k[1],c,Aj,cf,e)}return 0}return W(k[1],a,Ak,Bw,b,c,Bv)},o=function(a,b){return bH(bV,a,b)},p=function(a,b){return bH(bV,a,b)};return l7(k[1],c,Bx,p,m,o,l,n,j)}}function
k2(h,g,f,e,a){if(a){var
i=a[1],m=i[2],n=i[1],c=k2(h,g,f,e,a[2]),j=c[3],k=c[2],l=c[1];try{var
d=kU(h,g,k,j,m),p=[0,[0,[0,n,d[1]],l],d[2],d[3]];return p}catch(a){a=o(a);if(b(cI[18],a))return[0,l,k,j];throw a}}return[0,0,f,e]}function
k3(d,c,h,g,f){var
a=kU(d,c,h,b(bw[4],0),f),i=a[1],e=k2(d,c,a[2],a[3],g);return[0,e[1],i,e[2]]}var
e7=[i,function(c){var
a=h(kp),b=h(aP);return[0,h(aP),b,ap,a,k1]}],e8=[i,function(d){function
a(a){return cL(dx,ce,a)}var
b=h(g2),c=h(dx);return[0,h(dx),c,ce,b,a]}];function
By(d,c){function
e(a){return(2*a|0)+1|0}return iG(function(d,c,g){var
i=b(ag[3],c),f=b(ag[3],d)+i|0;if(g){if(0===g[1]){var
j=[1,a(u[2],d,c)],k=-(2*e(f)|0)|0,l=b(bw[4],k),m=df([1,j]);return[0,l,dA(h(aP),ap,m)]}var
n=[0,a(u[2],d,c)],o=-e(e(f))|0,p=b(bw[4],o),q=df([0,n]);return[0,p,dA(h(aP),ap,q)]}var
r=[1,a(u[2],d,c)],s=[0,a(u[2],d,c)],t=b(bw[4],-(2*(2*f|0)|0)|0),v=f0(c,r,s);return[0,t,dA(h(aP),ap,v)]},d,c)}function
Bz(a,g,f,e,d){var
i=[0,a[2]],k=[0,h(kE),i],c=b(j[23],k),l=a[3],m=a[2],n=kV(c,function(a){return dA(m,l,a)},d),o=k0(e),p=e5(a[1],o);function
q(d){var
e=b(aZ[35][6],d),i=[0,a[1]],k=[0,h(Bq),i],l=[0,[0,BB,p,b(j[23],k)],[0,[0,BA,g,f],0]],m=[0,h(kD),[0,c]],o=kZ([0,[0,BC,n,b(j[23],m)],l],e),q=[0,b(aF[54],o),0];return b(N[65][22],q)}return b(cM[68][8],q)}function
bW(c,a){if(typeof
c==="number"){if(0===c){if(typeof
a==="number")if(0===a)return 0}else
if(typeof
a==="number")if(0!==a)return 1}else
switch(c[0]){case
0:return[0,c[1]];case
1:if(typeof
a!=="number"&&1===a[0])return a;break;case
2:if(typeof
a!=="number"&&2===a[0]){var
d=a[1],f=c[1],g=bW(c[2],a[2]);return[2,bW(f,d),g]}break;case
3:if(typeof
a!=="number"&&3===a[0]){var
h=a[1],i=c[1],j=bW(c[2],a[2]);return[3,bW(i,h),j]}break;case
4:if(typeof
a!=="number"&&4===a[0])return[4,bW(c[1],a[1])];break;default:if(typeof
a!=="number"&&5===a[0]){var
k=a[2],l=a[1],m=c[1],n=bW(c[3],a[3]);return[5,bW(m,l),k,n]}}return b(e[3],BI)}var
hb=[aS,BJ,aR(0)];function
k4(b,a){var
c=[0,a,0];function
d(c,b){var
d=b[2],e=b[1],a=c[2],f=c[1];if(typeof
a!=="number"&&0===a[0])return[0,e,d];return[0,[5,a,[0,f],e],[0,f,d]]}return g(f[21],d,b,c)}function
BK(t,r,q,l,T,F,E,S){var
m=k4(F,E)[1],x=b(bw[4],0),z=c7(function(c,b){return a(bw[3],c,b[1])},m,x),A=1+b(bw[5],z)|0,u=b(y[1],A),v=b(r,a(t,u,m)),n=v[1],G=v[2];function
p(g){if(g){var
h=g[1],d=p(g[2]);if(typeof
d==="number")return 0;else{if(0===d[0]){var
n=d[1],j=function(a){return a[1]},k=a(f[17],j,h),m=[0,b(l[2],0),k],c=b(l[3],m),e=typeof
c==="number"?0:0===c[0]?[0,[0,c[1],l]]:[1,c[1]];return typeof
e==="number"?0:0===e[0]?[0,[0,e[1],n]]:[1,[0,e[1],h]]}var
i=d[1];return[1,[0,i[1],i[2]]]}}return BD}var
c=p(n);if(typeof
c==="number")return 0;else{if(0===c[0]){var
w=c[1],H=a(f[47],n,w),I=function(a){return a[1]},J=a(f[17],I,G),K=b8[1],M=function(c,b){return a(b8[4],b,c)},N=g(f[20],M,K,J),O=function(e,c){var
d=c[2],h=c[1],i=b8[1],j=b(d[2][4],d[1]);function
k(c,b){var
d=a(f[7],h,c)[2][1];return a(b8[4],d,b)}var
l=g(s[15],k,j,i);return a(b8[7],e,l)},P=g(f[20],O,N,H),d=function(c){if(typeof
c==="number")return 0===c?0:1;else
switch(c[0]){case
0:return[0,c[1]];case
1:var
o=c[2],p=o[2],q=o[1],s=c[1];return a(b8[3],q,P)?[1,s,[0,q,p]]:[0,p];case
2:var
t=c[2],f=d(c[1]),i=d(t);if(typeof
f!=="number"&&0===f[0])if(typeof
i!=="number"&&0===i[0]){var
u=[0,f[1],i[1]],v=[0,h(gW),u];return[0,b(j[23],v)]}return[2,f,i];case
3:var
w=c[2],k=d(c[1]),l=d(w);if(typeof
k!=="number"&&0===k[0])if(typeof
l!=="number"&&0===l[0]){var
x=[0,k[1],l[1]],y=[0,h(gX),x];return[0,b(j[23],y)]}return[3,k,l];case
4:var
m=d(c[1]);if(typeof
m!=="number"&&0===m[0]){var
z=[0,m[1]],A=[0,h(kh),z];return[0,b(j[23],A)]}return[4,m];default:var
r=c[2],B=c[3],n=d(c[1]),e=d(B);if(typeof
n!=="number"&&0===n[0]){var
C=n[1];if(r)return e;if(typeof
e!=="number"&&0===e[0])return[0,g(j[35],C,0,e[1])]}return[5,n,r,e]}},i=d(m),Q=b(r,a(t,u,i))[1],B=a(f[47],n,w),C=function(m){try{var
y=function(c){var
e=c[2],f=c[1],i=b(e[2][4],e[1]);function
d(g,f){var
c=g,b=f;for(;;){if(b){var
e=b[2],h=b[1];if(a(s[3],c,i))return[0,h,d(c+1|0,e)];var
c=c+1|0,b=e;continue}return 0}}return iR(X,d(0,f),m)},z=a(f[33],y,B),c=z}catch(a){a=o(a);if(a!==L)throw a;b(k[2],BG);b(e[52],e[28]);var
c=b(e[3],BH)}var
n=c[2],d=n[2],x=c[1],p=n[1];function
q(b,a){return[0,a[1],b]}var
i=a(f[18],q,m);function
r(d){try{var
g=a(f[7],x,d)[1],c=g}catch(a){a=o(a);if(a[1]!==e9)throw a;var
c=b(e[3],BE)}return a(f[38],c,i)}try{var
w=a(d[5],p,r),l=w}catch(c){c=o(c);if(!b(cI[18],c))throw c;var
t=function(a){return a[1]},u=a(f[17],t,i),v=[0,b(d[2],0),u],g=b(d[3],v);if(typeof
g==="number")var
h=0;else
if(0===g[0])var
j=g[1],h=1;else
var
h=0;if(!h)var
j=b(e[3],BF);var
l=j}return l},D=a(f[17],C,Q),R=fM(i);return[0,[0,R,i,g7(q[4],q[5],D)]]}return[1,c[1]]}}function
k5(j,i,h,g,f,d,c,a){try{var
k=BK(j,i,h,g,f,d,c,a);return k}catch(a){a=o(a);if(a===L){b(d7[4],e[28]);b(e[52],e[28]);return 0}throw a}}function
k6(d,c,a){var
e=b(cM[68][4],a);return g(aF[13],d,c,e)}function
bX(A,z,y,x,w,v,u){function
c(c){var
B=b(aZ[35][4],c),D=b(aZ[35][6],c),E=b(aZ[35][14],c);try{var
d=[0,b(aZ[35][5],c),B],i=k3(d,A,eY(d),E,D),q=i[3][1],L=i[2],O=i[1],k=h(x),P=h(w),l=k5(z,y,k,v,q,O,L,d);if(typeof
l==="number"){b(e[52],e[28]);var
Q=b(bi[3],BR),m=a(N[65][4],0,Q)}else
if(0===l[0])var
n=l[1],r=n[2],R=n[3],S=n[1],g=kY(d,P,r),p=g[3],T=g[4],U=g[2],V=g[1],s=function(a){return b(aF[2],a[1])},W=a(f[17],s,p),X=b(N[65][22],W),Y=a(f[17],s,U),Z=b(N[65][22],Y),_=function(b){return[0,a(k7[1],0,[1,[0,b]])]},$=b(bU[1][6],BS),t=k6(bU[1][10][1],$,c),aa=function(a){var
c=a[2];return[0,b(j[11],a[1]),c]},ab=a(f[17],aa,p),ac=[0,k[4]],ad=[0,h(kj),ac],ae=[0,Z,[0,X,[0,Bz(k,R,b(j[23],ad),ab,T),0]]],af=b(N[65][22],ae),ag=ha(d,r)[1],ah=b(f[9],ag),ai=function(b){return a(f[7],q,b[2]-1|0)},aj=a(f[17],ai,p),ak=a(e[26],ah,aj),al=a(N[65][3],af,u),am=b(aF[79],0),an=a(N[65][3],am,al),ao=[0,b(j[11],t),ak],ap=b(j[40],ao),aq=[0,b(aF[46],ap),0],ar=a(f[17],j[11],S),as=[0,b(aF[me],ar),aq],at=[0,an,[0,b(N[65][22],as),0]],au=_(t),av=C(aF[m1],1,BT,au,V),m=a(N[65][21],av,at);else
var
aw=b(bi[3],BU),m=a(N[65][4],0,aw);return m}catch(c){c=o(c);if(c===M){var
F=b(bi[3],BL);return a(N[65][4],0,F)}if(c===eG){var
G=b(bi[3],BM);return a(N[65][4],0,G)}if(c===hb){b(e[52],e[28]);var
H=a(e[17],BO,BN),I=a(e[17],BP,H),J=a(e[17],BQ,I),K=b(bi[3],J);return a(N[65][4],0,K)}throw c}}return b(cM[68][8],c)}function
BV(g,f,e){var
a=h(km),c=h(eS),i=[0,h(g2)],k=[0,h(kj),i],l=b(j[23],k),m=[0,h(kE),[0,a]],d=b(j[23],m),n=kV(d,function(b){return dA(a,bg,b)},e),o=e5(c,k0(f));function
p(a){var
e=b(aZ[35][6],a),f=[0,V(BZ,BY,BX),[0,c]],i=[0,[0,B0,o,b(j[23],f)],[0,[0,BW,g,l],0]],k=[0,h(kD),[0,d]],m=kZ([0,[0,B1,n,b(j[23],k)],i],e),p=[0,b(aF[54],m),0];return b(N[65][22],p)}return b(cM[68][8],p)}function
e_(au){return function(av){var
x=[i,function(d){function
a(a){return cL(dx,ce,a)}var
b=h(g2),c=h(km);return[0,h(eS),c,ce,b,a]}];function
c(c){var
y=b(aZ[35][4],c),z=b(aZ[35][6],c),A=b(aZ[35][14],c);try{var
d=[0,b(aZ[35][5],c),y],i=k3(d,A2,eY(d),A,z),p=i[2],q=i[1],r=i[3][1],I=h(x),J=function(a){var
b=a[2],c=a[1];return[0,c,a1(function(a){return d0(aD,a)},b)]},K=a(f[17],J,q),L=a1(function(a){return d0(aD,a)},p),k=k5(function(b,a){return a},cw,I,au,r,K,L,d);if(typeof
k==="number")var
n=0;else
if(0===k[0])var
l=k[1],P=l[3],Q=l[2],R=l[1],S=function(b){return a(f[31],b[1],R)},t=k4(a(f[35],S,q),p),T=t[2],u=bW(Q,t[1]),g=kY(d,h(A8),u),m=g[3],U=g[4],V=g[2],W=g[1],v=function(a){return b(aF[2],a[1])},X=a(f[17],v,m),Y=b(N[65][22],X),Z=a(f[17],v,V),_=b(N[65][22],Z),$=function(b){return[0,a(k7[1],0,[1,[0,b]])]},aa=b(bU[1][6],B9),w=k6(bU[1][10][1],aa,c),ab=function(a){var
c=a[2];return[0,b(j[11],a[1]),c]},ac=[0,_,[0,Y,[0,BV(P,a(f[17],ab,m),U),0]]],ad=b(N[65][22],ac),ae=ha(d,u)[1],af=b(f[9],ae),ag=function(b){return a(f[7],r,b[2]-1|0)},ah=a(f[17],ag,m),ai=a(e[26],af,ah),aj=a(N[65][3],ad,av),ak=b(aF[79],0),al=a(N[65][3],ak,aj),am=[0,b(j[11],w),ai],an=b(j[40],am),ao=[0,b(aF[46],an),0],ap=a(f[17],j[11],T),aq=[0,b(aF[me],ap),ao],ar=[0,al,[0,b(N[65][22],aq),0]],as=$(w),at=C(aF[m1],1,B_,as,W),s=a(N[65][21],at,ar),n=1;else
var
n=0;if(!n){b(e[52],e[28]);var
O=b(bi[3],B8),s=a(N[65][4],0,O)}return s}catch(c){c=o(c);if(c===M){var
B=b(bi[3],B2);return a(N[65][4],0,B)}if(c===eG){var
D=b(bi[3],B3);return a(N[65][4],0,D)}if(c===hb){b(e[52],e[28]);var
E=a(e[17],B5,B4),F=a(e[17],B6,E),G=a(e[17],B7,F),H=b(bi[3],G);return a(N[65][4],0,H)}throw c}}return b(cM[68][8],c)}}var
B$=eR([0,X,bc[27]]),Cd=b(Cc[8],Cb)?0:[i,function(a){throw hb}];function
Cj(i){var
l=i[2],m=i[1];h(Cd);var
j=[0,Ch,[0,Cg,[0,a(e[17],Cf,Ce[22]),0]]],k=b(Ci[3],0),d=g(f[20],bD[4],k,j),c=i1(d,[0,d],[0,m,l]);if(0===c[0])return c[1];throw b(e[3],c[1])}var
Ck=a(B$[4],Ca,Cj);function
k8(c,a){return b(Ck,[0,c,a])}function
e$(a){switch(a[0]){case
0:return[0,[0,a[1],0]];case
1:var
b=a[1];return[1,b,e$(a[2])];default:var
c=a[2],d=a[1],e=e$(a[3]);return[2,e$(d),c,e]}}function
k9(f,a){var
c=k8(f,a);if(c){var
d=j3(c[1]);return f2(a,d)?[0,d]:(b(e[31],Cl),0)}return 0}function
dB(e,d,c){function
f(i,h){var
c=i,d=h;for(;;){if(typeof
c!=="number")switch(c[0]){case
0:var
g=b(ag[5],c[1]);return e<=g?a(s[4],g-e|0,d):d;case
2:var
c=c[2];continue;case
3:case
4:var
j=c[1],k=f(c[2],d),c=j,d=k;continue}return d}}return f(c,d)}function
dC(a){return dB(0,s[1],a)}function
bI(a,d){function
c(a){if(typeof
a!=="number")switch(a[0]){case
0:var
e=b(d,b(ag[5],a[1]));return[0,b(y[4],e)];case
2:var
f=a[1];return[2,f,c(a[2])];case
3:var
g=a[1],h=c(a[2]);return[3,c(g),h];case
4:var
i=a[1],j=c(a[2]);return[4,c(i),j]}return a}return c(a)}function
hc(a){function
d(i,h,e){var
b=i,a=h,c=e;for(;;)if(typeof
a==="number")return c;else
switch(a[0]){case
0:var
j=a[2],k=dB(b,c,a[1]),b=b+1|0,a=j,c=k;continue;case
1:var
l=a[2],m=dB(b,c,a[1]),b=b+1|0,a=l,c=m;continue;default:var
n=a[3],o=a[1],p=dB(b,dB(b,c,a[2]),o),q=function(c,a){return d(b+1|0,a,c)};return g(f[20],q,p,n)}}return d(0,a,s[1])}function
hd(a,e){function
c(c,a){return a<c?a:b(e,a-c|0)+c|0}function
d(b,a){if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
e=a[1],f=d(b+1|0,a[2]);return[0,bI(e,function(a){return c(b,a)}),f];case
1:var
g=a[1],h=d(b+1|0,a[2]);return[1,bI(g,function(a){return c(b,a)}),h];default:var
i=a[3],j=a[2],k=a[1],l=ba(function(a){return d(b+1|0,a)},i),m=bI(j,function(a){return c(b,a)});return[2,bI(k,function(a){return c(b,a)}),m,l]}}return d(0,a)}function
dD(d,c){function
e(a){var
b=a[2];return[0,it(a[1]),b]}return b(d,a(f[17],e,c))}var
k_=eR([0,X,bc[27]]),Cn=eR([0,X,bc[27]]);function
Co(a){var
b=a[1],c=a[2],d=b[3],e=b[2];return dD(function(a){return j_(e,d,a)},c)}var
Cq=a(k_[4],Cp,Co);function
Cr(a){var
b=a[1],c=a[2],d=b[3],e=b[2];return dD(function(a){return j$(e,d,a)},c)}var
Ct=a(k_[4],Cs,Cr);function
Cu(a){var
b=a[2],c=a[1];return dD(function(a){return j0(c,a)},b)}var
Cw=a(Cn[4],Cv,Cu);function
Cx(b,a){return bh(bj,b,a[1])}function
Cy(a,b){return bH(bj,a,b)}var
CA=[0,Cz,gV,function(a){var
b=a[2],c=a[1];return dD(function(a){return gO(c,a)},b)},dC,bI,Cy,Cx];function
CB(b,a){return bh(bj,b,a[1])}function
CC(a,b){return bH(bj,a,b)}var
CE=[0,CD,gV,function(a){var
b=a[2],c=a[1];return dD(function(a){return gO(c,a)},b)},dC,bI,CC,CB];function
CF(b,a){return bh(bj,b,a[1])}var
k$=[0,CG,gV,Cw,dC,bI,function(a,b){return bH(bj,a,b)},CF];function
la(b,a){function
c(b,a){return bh(bj,b,a[1])}function
d(a,b){return bH(bj,a,b)}function
e(a){return k9(a[1],a[2])}return[0,CH,function(c){return[0,b,a]},e,dC,bI,d,c]}function
lb(b,a){function
c(b,a){return bh(bj,b,a[1])}function
d(a,b){return bH(bj,a,b)}function
e(a){return k9(a[1],a[2])}return[0,CI,function(c){return[0,b,a]},e,dC,bI,d,c]}function
lc(d,c){function
g(b,a){return bh(bV,b,a[1])}function
h(h){var
i=h[2],k=h[1];function
j(a){var
b=a[2];return[0,e$(a[1]),b]}var
d=k8(k,a(f[17],j,i));if(d)var
g=j5(d[1]),c=iv(i,g)?[0,g]:(b(e[31],Cm),b(e[52],e[28]),0);else
var
c=0;if(typeof
c!=="number"&&0===c[0])return[0,[0,c[1],0]];return 0}return[0,CJ,function(a){return[0,d,c]},h,hc,hd,cf,g]}var
CL=[0,CK,ke,Cq,hc,hd,cf,function(b,a){return bh(bV,b,a[1])}],CN=[0,CM,ke,Ct,hc,hd,cf,function(b,a){return bh(bV,b,a[1])}];function
CO(b,a){return a}function
ld(a){return bX(e1,CO,cw,e8,e4,CA,a)}function
he(a){var
b=la(CP,[0,a]);function
c(b,a){return a}return function(a){return bX(e1,c,cw,e8,e4,b,a)}}var
le=e_(CE);function
hf(a){return e_(lb(CQ,[0,a]))}function
hg(a){var
b=lc(CR,[0,a]);function
c(b,a){return a}return function(a){return bX(e0,c,db,e7,e3,b,a)}}var
CT=lc(CS,0);function
CU(b,a){return a}function
lf(a){return bX(e0,CU,db,e7,e3,CT,a)}var
CW=la(CV,0);function
CX(b,a){return a}function
lg(a){return bX(e1,CX,cw,e8,e4,CW,a)}var
lh=e_(lb(CY,0));function
li(a){return bX(e0,By,db,e7,e3,CL,a)}function
CZ(b,a){return a}function
lj(a){return bX(e0,CZ,db,e7,e3,CN,a)}var
lk=e_(k$);function
C0(b,a){return a}function
ll(a){return bX(e1,C0,cw,e8,e4,k$,a)}function
lm(d){function
c(c){var
e=b(aZ[35][4],c);if(g6(b(aZ[35][5],c),e,d))return N[65][2];var
f=b(bi[3],C1);return a(N[65][4],0,f)}return b(cM[68][8],c)}aA(1038,[0,lm,hg,he,hf,li,lj,lk,ll,lf,lg,lh,ld,le,k1],"Micromega_plugin__Coq_micromega");b(C2[9],ax);var
C3=0,C5=[0,[0,C4,function(a){return aF[56]}],C3];T(aG[8],ax,C6,0,0,C5);var
C7=0;function
C8(a,b){return lm(a)}var
C_=[0,[0,[0,C9,[1,[5,b(aa[16],fa[11])],0]],C8],C7];T(aG[8],ax,C$,0,0,C_);var
Da=0;function
Db(d,c){var
e=a(ay[24],c,d);return b(hg(-1),e)}var
Dd=[0,[0,[0,Dc,[1,[5,b(aa[16],az[9])],0]],Db],Da];function
De(e,d,c){var
f=a(ay[24],c,d);return b(hg(e),f)}var
Df=[1,[5,b(aa[16],az[9])],0],Dh=[0,[0,[0,Dg,[1,[5,b(aa[16],fa[6])],Df]],De],Dd];T(aG[8],ax,Di,0,0,Dh);var
Dj=0;function
Dk(c,b){return li(a(ay[24],b,c))}var
Dm=[0,[0,[0,Dl,[1,[5,b(aa[16],az[9])],0]],Dk],Dj];T(aG[8],ax,Dn,0,0,Dm);var
Do=0;function
Dp(c,b){return lj(a(ay[24],b,c))}var
Dr=[0,[0,[0,Dq,[1,[5,b(aa[16],az[9])],0]],Dp],Do];T(aG[8],ax,Ds,0,0,Dr);var
Dt=0;function
Du(d,c){return b(lk,a(ay[24],c,d))}var
Dw=[0,[0,[0,Dv,[1,[5,b(aa[16],az[9])],0]],Du],Dt];T(aG[8],ax,Dx,0,0,Dw);var
Dy=0;function
Dz(c,b){return ll(a(ay[24],b,c))}var
DB=[0,[0,[0,DA,[1,[5,b(aa[16],az[9])],0]],Dz],Dy];T(aG[8],ax,DC,0,0,DB);var
DD=0;function
DE(c,b){return lf(a(ay[24],b,c))}var
DG=[0,[0,[0,DF,[1,[5,b(aa[16],az[9])],0]],DE],DD];T(aG[8],ax,DH,0,0,DG);var
DI=0;function
DJ(c,b){return lg(a(ay[24],b,c))}var
DL=[0,[0,[0,DK,[1,[5,b(aa[16],az[9])],0]],DJ],DI];T(aG[8],ax,DM,0,0,DL);var
DN=0;function
DO(d,c){return b(lh,a(ay[24],c,d))}var
DQ=[0,[0,[0,DP,[1,[5,b(aa[16],az[9])],0]],DO],DN];T(aG[8],ax,DR,0,0,DQ);var
DS=0;function
DT(c,b){return ld(a(ay[24],b,c))}var
DV=[0,[0,[0,DU,[1,[5,b(aa[16],az[9])],0]],DT],DS];T(aG[8],ax,DW,0,0,DV);var
DX=0;function
DY(d,c){return b(le,a(ay[24],c,d))}var
D0=[0,[0,[0,DZ,[1,[5,b(aa[16],az[9])],0]],DY],DX];T(aG[8],ax,D1,0,0,D0);var
D2=0;function
D3(d,c){var
e=a(ay[24],c,d);return b(hf(-1),e)}var
D5=[0,[0,[0,D4,[1,[5,b(aa[16],az[9])],0]],D3],D2];function
D6(e,d,c){var
f=a(ay[24],c,d);return b(hf(e),f)}var
D7=[1,[5,b(aa[16],az[9])],0],D9=[0,[0,[0,D8,[1,[5,b(aa[16],fa[6])],D7]],D6],D5];T(aG[8],ax,D_,0,0,D9);var
D$=0;function
Ea(d,c){var
e=a(ay[24],c,d);return b(he(-1),e)}var
Ec=[0,[0,[0,Eb,[1,[5,b(aa[16],az[9])],0]],Ea],D$];function
Ed(e,d,c){var
f=a(ay[24],c,d);return b(he(e),f)}var
Ee=[1,[5,b(aa[16],az[9])],0],Eg=[0,[0,[0,Ef,[1,[5,b(aa[16],fa[6])],Ee]],Ed],Ec];T(aG[8],ax,Eh,0,0,Eg);aA(1045,[0],"Micromega_plugin__G_micromega");function
dE(b,a){return 0===fv(b,a)?1:0}function
dF(b,a){return fv(b,a)<0?1:0}function
Ei(b,a){return fv(b,a)<=0?1:0}function
aj(d,c,a){return b(d,b(c,a))}function
dG(b){return a(d[14],Ej,[0,b])}function
fb(b){return a(d[14],Ek,[0,b])}function
ln(c){var
e=b(d[54],c),a=b(dg[5],e),f=b(dg[3],a),g=b(d[51],f),h=b(dg[2],a);return[0,b(d[51],h),g]}function
El(a){return a[1]}function
hi(a){return aj(El,ln,a)}function
Em(a){return a[2]}function
fc(a){return aj(Em,ln,a)}function
fd(e,c){var
f=b(d[52],c),g=b(d[52],e),h=a(l[17],g,f);return b(d[51],h)}function
fe(e,c){if(a(d[26],e,hh))if(a(d[26],c,hh))return hh;var
f=fd(e,c),g=a(d[6],e,c),h=a(d[9],g,f);return b(d[15],h)}function
bl(d,c){if(c){var
f=c[2],g=c[1];return f?a(d,g,bl(d,f)):g}return b(e[3],En)}function
ff(d,b,c){if(b){var
e=b[1],h=ff(d,b[2],c),i=function(c,b){return[0,a(d,e,c),b]};return g(f[21],i,c,h)}return 0}function
fg(a){return g(f[21],e[17],a,Eo)}function
a8(c){var
a=cn(c)-1|0,b=0;for(;;){if(0<=a){var
d=[0,g(a9[4],c,a,1),b],a=a-1|0,b=d;continue}return b}}function
hj(f,e,d){var
c=f,a=d;for(;;){if(1<=c){var
c=c-1|0,a=b(e,a);continue}return a}}function
Q(a,b){return b<a?0:[0,a,Q(a+1|0,b)]}function
hk(d,c){var
a=c;for(;;){if(a){var
f=a[2],g=a[1];try{var
h=b(d,g);return h}catch(b){b=o(b);if(b[1]===e9){var
a=f;continue}throw b}}return b(e[3],Ep)}}function
lo(d,c){var
a=c;for(;;){if(a){var
e=a[2],b=dE(d,a[1]);if(b)return b;var
a=e;continue}return 0}}function
Eq(b,a){return lo(b,a)?a:[0,b,a]}function
hl(b,a){return g(f[21],Eq,b,a)}function
hm(c,b){function
d(a){return 1-lo(a,b)}return a(f[35],d,c)}function
dH(a,d,c){var
e=b(a,c);return dF(b(a,d),e)}function
cN(d,c){var
a=c;for(;;){if(a){var
e=a[2];b(d,a[1]);var
a=e;continue}return 0}}function
aQ(d,c){if(c){var
g=c[1],i=c[2],j=b(d,g),h=a(f[37],j,i),k=h[2],l=[0,g,aQ(d,h[1])],m=aQ(d,k);return a(e[26],m,l)}return 0}function
lp(a){if(a){var
b=a[2];if(b){var
d=a[1],e=b[1],c=lp(b);return dE(d,e)?c:c===b?a:[0,d,c]}}return a}function
cO(a){return lp(aQ(Ei,a))}var
q=0;function
aq(a){return typeof
a==="number"?1:0}function
lq(c,a){if(a){var
d=a[1],e=d[2],f=d[1],g=lq(c,a[2]);return[0,[0,f,b(c,e)],g]}return 0}function
aH(b,a){if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=a[1];return[0,c,lq(b,a[2])]}var
d=a[3],e=a[2],f=a[1],g=aH(b,a[4]);return[1,f,e,aH(b,d),g]}}function
A(f,j,i){var
c=j,a=i;for(;;)if(typeof
a==="number")return c;else{if(0===a[0]){var
d=c,b=a[2];for(;;){if(b){var
e=b[1],h=b[2],d=g(f,d,e[1],e[2]),b=h;continue}return d}}var
k=a[4],c=A(f,c,a[3]),a=k;continue}}function
lr(c,a,b){if(a){var
d=a[1],e=d[2],f=d[1];return g(c,f,e,lr(c,a[2],b))}return b}function
dI(c,e,d){var
a=e,b=d;for(;;)if(typeof
a==="number")return b;else{if(0===a[0])return lr(c,a[2],b);var
f=a[3],g=dI(c,a[4],b),a=f,b=g;continue}}function
cg(b,e,g,d){var
c=b^g,a=c&(-c|0),f=b&(a-1|0);return 0===(b&a)?[1,f,a,e,d]:[1,f,a,d,e]}function
ls(a,b){var
c=a[1];if(b){var
d=b[2],e=b[1],f=e[1];return dE(c,f)?[0,a,d]:dF(c,f)?[0,a,b]:[0,e,ls(a,d)]}return[0,a,0]}function
fh(f,e,d,c){if(d){if(c){var
j=c[2],g=c[1],k=g[1],l=d[2],h=d[1],i=h[1],o=g[2],p=h[2];if(dF(i,k))return[0,h,fh(f,e,l,c)];if(dF(k,i))return[0,g,fh(f,e,d,j)];var
m=a(f,p,o),n=fh(f,e,l,j);return b(e,m)?n:[0,[0,i,m],n]}return d}return c}function
E(d,e){var
c=b(bc[27],d);function
g(a){if(typeof
a==="number")return[0,c,[0,[0,d,e],0]];else{if(0===a[0]){var
h=a[1],k=a[2];return h===c?[0,h,ls([0,d,e],k)]:cg(h,a,c,[0,c,[0,[0,d,e],0]])}var
i=a[4],j=a[3],b=a[2],f=a[1];return(c&(b-1|0))!==f?cg(f,a,c,[0,c,[0,[0,d,e],0]]):0===(c&b)?[1,f,b,g(j),i]:[1,f,b,j,g(i)]}}return g}function
aI(e,d,b,a){if(typeof
b==="number")return a;else
if(0===b[0]){var
m=b[1],F=b[2];if(typeof
a==="number")var
t=0;else{if(0===a[0]){var
w=a[1],G=a[2];if(m===w){var
x=fh(e,d,F,G);return 0===x?0:[0,m,x]}return cg(m,b,w,a)}var
y=a,q=a[4],p=a[3],j=a[2],i=a[1],o=b,n=m,t=1}}else{var
k=b[4],l=b[3],g=b[2],c=b[1];if(typeof
a==="number")var
t=0;else{if(0!==a[0]){var
r=a[4],s=a[3],h=a[2],f=a[1];if(g<h){if((f&(g-1|0))!==c)return cg(c,b,f,a);if(0===(f&g)){var
B=aI(e,d,l,a);return aq(B)?k:[1,c,g,B,k]}var
C=aI(e,d,k,a);return aq(C)?l:[1,c,g,l,C]}if(h<g){if((c&(h-1|0))!==f)return cg(c,b,f,a);if(0===(c&h)){var
D=aI(e,d,b,s);return aq(D)?r:[1,f,h,D,r]}var
E=aI(e,d,b,r);return aq(E)?s:[1,f,h,s,E]}if(c===f){var
u=aI(e,d,l,s),v=aI(e,d,k,r);return aq(u)?v:aq(v)?u:[1,c,g,u,v]}return cg(c,b,f,a)}var
y=b,q=k,p=l,j=g,i=c,o=a,n=a[1],t=1}}if(t){if((n&(j-1|0))===i){if(0===(n&j)){var
z=aI(e,d,o,p);return aq(z)?q:[1,i,j,z,q]}var
A=aI(e,d,o,q);return aq(A)?p:[1,i,j,p,A]}return cg(n,o,i,y)}return b}function
a0(c,a){return b(E(c,a),q)}function
hn(c){var
a=c;for(;;)if(typeof
a==="number")return b(e[3],Er);else{if(0===a[0])return b(f[5],a[2]);var
a=a[3];continue}}function
lt(k,e,c){var
h=b(bc[27],c),a=k;for(;;){if(typeof
a!=="number"){if(0!==a[0]){var
m=a[4],n=a[3],o=0===(h&a[2])?n:m,a=o;continue}var
l=a[2];if(a[1]===h){var
d=l;for(;;){if(d){var
f=d[1],g=f[1],i=d[2],j=f[2];if(dE(c,g))return j;if(0<fv(c,g)){var
d=i;continue}return b(e,c)}return b(e,c)}}}return b(e,c)}}function
fi(a){function
c(a){return b(e[3],Es)}return function(b){return lt(a,c,b)}}function
a_(c,b,a){return lt(c,function(b){return a},b)}function
lu(b,a){if(a){var
c=a[2],d=a[1],e=d[1];if(dE(b,e))return c;if(dF(b,e))return a;var
f=lu(b,c);return f===c?a:[0,d,f]}return 0}function
ho(k){var
e=b(bc[27],k);function
f(a){if(typeof
a!=="number")if(0===a[0]){var
l=a[2],m=a[1];if(m===e){var
g=lu(k,l);return g===l?a:0===g?0:[0,m,g]}}else{var
b=a[4],c=a[3],d=a[2],h=a[1];if((e&(d-1|0))===h){if(0===(e&d)){var
i=f(c);return i===c?a:aq(i)?b:[1,h,d,i,b]}var
j=f(b);return j===b?a:aq(j)?c:[1,h,d,c,j]}}return a}return f}function
ch(a){var
b=0;return cO(A(function(c,b,a){return[0,[0,b,a],c]},b,a))}function
fj(a){var
b=0;return cO(A(function(b,a,c){return[0,a,b]},b,a))}var
cP=[aS,Et,aR(0)];function
Eu(a){return bL(a,0)}var
Ev=a(e[17],lz,lA),Ew=a(e[17],ly,Ev),Ex=a(e[17],lx,Ew),Ey=a(e[17],lw,Ex),EA=a8(a(e[17],lv,Ey)),Ez=256,EB=e[6];function
EC(a){return aj(EB,Eu,a)}var
bY=B.caml_make_vect(g(f[21],EC,EA,Ez),0),ED=a8(lv);cN(function(b){var
a=bL(b,0);return H(bY,a)[a+1]=1},ED);var
EE=a8(lw);cN(function(b){var
a=bL(b,0);return H(bY,a)[a+1]=2},EE);var
EF=a8(lx);cN(function(b){var
a=bL(b,0);return H(bY,a)[a+1]=4},EF);var
EG=a8(ly);cN(function(b){var
a=bL(b,0);return H(bY,a)[a+1]=8},EG);var
EH=a8(lz);cN(function(b){var
a=bL(b,0);return H(bY,a)[a+1]=16},EH);var
EI=a8(lA);cN(function(b){var
a=bL(b,0);return H(bY,a)[a+1]=32},EI);function
hp(b){var
a=bL(b,0);return 1===H(bY,a)[a+1]?1:0}function
lB(b){var
a=bL(b,0);return 32===H(bY,a)[a+1]?1:0}function
fk(a,d,c){try{var
e=b(a,c);return e}catch(a){a=o(a);if(a===cP)return b(d,c);throw a}}function
ac(f,e,d){var
a=b(f,d),g=a[1],c=b(e,a[2]);return[0,[0,g,c[1]],c[2]]}function
ci(a,c){try{var
d=b(a,c),f=d[1],e=ci(a,d[2]),g=[0,[0,f,e[1]],e[2]];return g}catch(a){a=o(a);if(a===cP)return[0,0,c];throw a}}function
ak(e,d,c){var
a=b(e,c),f=a[2];return[0,b(d,a[1]),f]}function
EJ(f,d,c){try{var
h=b(d,c);return h}catch(c){c=o(c);if(c===cP){var
g=a(e[17],f,EK);return b(e[3],g)}throw c}}function
EL(a,c,b){function
d(a){return[0,a[1],a[2]]}function
e(a){return a[2]}function
f(c){return EJ(b,a,c)}function
g(a){return ac(c,f,a)}function
h(a){return ak(g,e,a)}function
i(a){return ci(h,a)}function
j(b){return ac(a,i,b)}return function(a){return ak(j,d,a)}}function
hq(d,c){try{var
a=b(d,c),e=[0,[0,a[1],0],a[2]];return e}catch(a){a=o(a);if(a===cP)return[0,0,c];throw a}}function
dJ(d,a){if(a){var
c=a[1],e=a[2];if(b(d,c))return[0,c,e];throw cP}throw cP}function
bm(a){function
b(b){return X(b,a)}return function(a){return dJ(b,a)}}function
fl(b,a,d){if(0<b)var
e=function(a){return[0,a[1],a[2]]},f=b-1|0,g=function(b){return fl(f,a,b)},h=function(b){return ac(a,g,b)},c=function(a){return ak(h,e,a)};else
var
c=function(b){return ci(a,b)};return c(d)}var
cj=b(bD[16],0);function
EM(g){try{var
j=b(e[1][67],g),d=j}catch(c){c=o(c);if(c[1]!==EN)throw c;var
h=a(e[17],EO,g),d=b(e[3],h)}function
c(g){try{var
a=c([0,b(e[1][71],d),g]);return a}catch(a){a=o(a);if(a===ka)return b(f[9],g);throw a}}var
i=c(0);b(e[1][81],d);return i}function
fm(b){var
c=EM(b);return a(a9[7],EP,c)}function
ck(f,d){var
c=b(e[1][48],f);a(e[55],c,d);return b(e[65],c)}function
EQ(d,a){var
c=a;for(;;)try{var
e=b(d,c);return e}catch(a){a=o(a);if(a[1]===e9){var
c=c+1|0;continue}throw a}}var
hr=[aS,ER,aR(0)];aA(1048,[0,aj,bk,fb,dG,fg,a8,hj,hk,q,aq,E,a0,hn,aI,Q,a_,fi,A,dI,aH,ho,fj,ch,hl,hm,aQ,cO,dH,ff,fd,fe,hi,fc,bl,ak,ac,bm,ci,dJ,hq,hp,fk,lB,fl,EL,cj,fm,ck,function(d,c,a){var
e=a$(d,0);if(-1===e)return EQ(c,a);if(0===e)throw hr;return function(e,c){var
a=c;for(;;)try{var
f=b(e,a);return f}catch(b){b=o(b);if(b[1]===e9){if(a===d)throw hr;var
a=a+1|0;continue}throw b}}(c,a)},hr],"Micromega_plugin__Sos_lib");var
hs=[aS,ES,aR(0)];function
ht(c){var
e=a(d[9],EU,ET),f=b(d[15],c);if(a(d[27],f,e))return ht(a(d[6],EV,c))-1|0;var
g=b(d[15],c);return a(d[30],g,EW)?ht(a(d[9],c,EX))+1|0:0}function
dK(j,c){if(a(d[26],c,EY))return EZ;var
h=b(d[15],c),g=ht(h),k=fb(-g|0),l=a(d[6],k,h),m=a(d[1],l,E0),n=fb(j),o=a(d[6],n,m),p=b(d[23],o);if(0===g)var
i=E1;else
var
u=b(e[22],g),i=a(e[17],E5,u);var
q=a8(b(d[40],p)),r=fg(b(f[6],q)),s=a(e[17],r,i),t=a(d[27],c,E2)?E3:E4;return a(e[17],t,s)}function
cQ(h,f,e,d){var
c=h,a=f,b=d;for(;;){if(a){var
i=a[2],j=g(e,a[1],c,b),c=c+1|0,a=i,b=j;continue}return b}}function
dL(h,g,f){var
c=h,b=f;for(;;){var
e=c[2],d=c[1];if(e<d)return b;var
c=[0,d+1|0,e],b=a(g,d,b);continue}}function
cR(f,e,c){return a(d[26],e,E6)?c:b(E(f,e),c)}function
bn(b,a){return a_(b[2],a,E7)}function
hu(c,a){var
d=a[2],e=a[1];return[0,e,A(function(e,d,a){return cR(d,b(c,a),e)},q,d)]}function
lC(a){return typeof
a[2]==="number"?1:0}function
fn(a){return[0,a,q]}function
lD(c,b){var
e=b[1];if(a(d[26],c,E9))return fn(e);var
f=b[2];return[0,e,aH(function(b){return a(d[6],c,b)},f)]}function
E_(a){var
c=b(f[1],a),d=Q(1,c);return[0,c,C(f[26],E,d,a,q)]}function
lE(a){var
b=aH(d[3],a[2]);return[0,a[1],b]}function
lF(d,a){var
c=a[1][2],e=a[2];return[0,c,A(function(c,a,e){var
f=a[2];return a[1]===d?b(E(f,e),c):c},q,e)]}function
Fc(a){return 0}function
Fd(b,a){return b+a|0}function
fo(a,b){return aI(Fd,Fc,a,b)}function
lG(b,a){return a_(a,b,0)}function
Fe(a){var
b=1;return A(function(c,b,e){var
a=X(b,q),d=a?c:a;return d},b,a)}function
cl(b){return a(d[26],b,Fg)?q:a0(q,b)}function
lH(b,c){return a(d[26],b,Fh)?q:aH(function(c){return a(d[6],b,c)},c)}function
dM(a){return aH(d[3],a)}function
cm(c,b){function
e(b){return a(d[26],b,Fi)}return aI(d[1],e,c,b)}function
hv(b,a){return cm(b,dM(a))}function
bJ(c,e){return A(function(g,f,c){var
h=a(d[26],c,Fj)?q:X(f,q)?aH(function(b){return a(d[6],c,b)},e):A(function(h,g,e){var
i=a(d[6],c,e);return b(E(fo(f,g),i),h)},q,e);return cm(h,g)},q,c)}function
fp(b,a){if(0===a)return cl(Fk);if(1===a)return b;var
d=fp(b,a/2|0),c=bJ(d,d);return 1===(a%2|0)?bJ(b,c):c}function
fq(b){var
c=0;return A(function(f,d,g){var
b=0,c=A(function(b,c,a){return a+b|0},b,d);return a(e[6],c,f)},c,b)}function
hw(a){var
b=0;return dI(function(b,c){var
a=fj(b);return function(b){return hl(a,b)}},a,b)}function
fr(b,a){var
c=a[1],d=b[1],e=bZ(d,c),h=a[2],i=b[2];if(e)var
f=e;else
var
g=X(d,c),f=g?B.caml_greaterthan(i,h):g;return f}function
lI(c){if(X(c,q))return Fm;var
d=0,h=aQ(fr,ch(c));function
i(c,j){var
d=c[2],f=c[1];if(1===d)var
g=f;else
var
h=b(e[22],d),i=a(e[17],Fl,h),g=a(e[17],f,i);return[0,g,j]}var
j=g(f[21],i,h,d);return a(a9[7],Fn,j)}function
lJ(g){var
c=g[2],f=g[1];if(X(c,q))return b(d[40],f);if(a(d[26],f,Fo))return lI(c);var
h=lI(c),i=a(e[17],Fp,h),j=b(d[40],f);return a(e[17],j,i)}function
Fq(h){if(X(h,q))return Fr;var
j=ch(h),k=aQ(function(o,n){var
i=n[1],j=o[1],h=X(j,i);if(h)return h;var
m=aQ(fr,ch(i)),b=aQ(fr,ch(j)),a=m;for(;;){if(a){if(b){var
c=a[1],d=b[1],k=a[2],l=b[2],e=fr(d,c);if(e)var
f=e;else{var
g=X(d,c);if(g){var
b=l,a=k;continue}var
f=g}return f}return 0}return 1}},j);function
l(g,f){var
c=f[2],h=f[1];if(a(d[27],c,Ft)){var
i=lJ([0,b(d[3],c),h]),j=a(e[17],Fu,i);return a(e[17],g,j)}var
k=lJ([0,c,h]),l=a(e[17],Fv,k);return a(e[17],g,l)}var
c=g(f[20],l,Fs,k),m=g(a9[4],c,0,3),i=g(a9[4],c,3,cn(c)-3|0),n=l4(m,Fx)?i:a(e[17],Fz,i),o=a(e[17],n,Fw);return a(e[17],Fy,o)}function
bK(a){if(typeof
a==="number")return q;else
switch(a[0]){case
0:return cl(a[1]);case
1:return a0(a0(a[1],1),Ff);case
2:return dM(bK(a[1]));case
3:var
b=a[1],f=b[1],g=bK(b[2]);return cm(bK(f),g);case
4:var
c=a[1],h=c[1],i=bK(c[2]);return hv(bK(h),i);case
5:var
d=a[1],j=d[1],k=bK(d[2]);return bJ(bK(j),k);default:var
e=a[1],l=e[2];return fp(bK(e[1]),l)}}function
lK(b){var
c=Q(1,b[1]);function
d(a){return bn(b,a)}var
g=20;function
h(a){return dK(g,a)}function
i(a){return aj(h,d,a)}var
j=a(f[17],i,c),k=a(a9[7],FB,j);return a(e[17],k,FA)}function
lL(b){var
c=a8(b),d=a(f[17],bm,c);return bl(function(c,b){function
d(b){return a(e[17],b[1],b[2])}function
f(a){return ac(c,b,a)}return function(a){return ak(f,d,a)}},d)}function
fs(a){function
b(a){return a[1][2]}function
c(a){return dJ(hp,a)}function
d(a){return ci(c,a)}var
e=lL(a);function
f(a){return dJ(hp,a)}function
g(a){return ci(f,a)}function
h(a){return ac(g,e,a)}function
i(a){return ac(h,d,a)}return function(a){return ak(i,b,a)}}function
lM(a){return dJ(lB,a)}var
FN=d[43];function
FO(a){return aj(FN,fg,a)}var
FP=1;function
FQ(a){return fl(FP,lM,a)}function
lN(a){return ak(FQ,FO,a)}function
FR(c){var
e=fb(b(f[1],c)),g=fg(c),h=b(d[43],g);return a(d[9],h,e)}var
FS=1;function
FT(a){return fl(FS,lM,a)}function
FU(a){return ak(FT,FR,a)}function
FV(c){var
b=c[2],e=c[1];if(b)if(!b[2])return a(d[1],e,b[1]);return e}function
FW(a){return a[2]}var
FY=bm(FX);function
FZ(a){return ac(FY,FU,a)}function
F0(a){return ak(FZ,FW,a)}function
F1(a){return hq(F0,a)}function
F2(a){return ac(lN,F1,a)}function
F3(a){return ak(F2,FV,a)}function
lO(a){function
b(a){return a[2]}var
c=bm(F4);function
e(b){return ac(c,a,b)}function
f(a){return ak(e,b,a)}function
g(b){return fk(f,a,b)}function
h(a){return a[2]}var
i=d[3];function
j(a){return aj(i,h,a)}var
k=bm(F5);function
l(b){return ac(k,a,b)}function
m(a){return ak(l,j,a)}return function(a){return fk(m,g,a)}}function
F6(a){return a[2]}var
F7=lO(lN),F9=bm(F8),F$=bm(F_);function
Ga(a){return fk(F$,F9,a)}function
Gb(a){return ac(Ga,F7,a)}function
Gc(a){return ak(Gb,F6,a)}function
Gd(c){var
b=c[2],e=c[1];if(b)if(!b[2]){var
f=a(d[14],Ge,b[1]);return a(d[6],e,f)}return e}function
Gf(a){return hq(Gc,a)}var
Gg=lO(F3);function
Gh(a){return ac(Gg,Gf,a)}function
lP(a){return ak(Gh,Gd,a)}fs(Gj);fs(Gk);fs(Gl);function
Gm(a){return Gn}fs(Go);lL(Gp);function
Gq(a){return a[1]}function
Gr(a){return aj(E_,Gq,a)}var
Gt=bm(Gs),Gv=bm(Gu);function
Gw(a){return ac(Gv,Gt,a)}function
Gx(a){return ac(Gw,Gm,a)}function
Gy(a){return[0,a[1],a[2]]}function
Gz(a){return a[2]}var
GB=bm(GA);function
GC(a){return ac(GB,lP,a)}function
GD(a){return ak(GC,Gz,a)}function
GE(a){return ci(GD,a)}function
GF(a){return ac(lP,GE,a)}function
GG(a){return ak(GF,Gy,a)}function
GH(a){return ac(GG,Gx,a)}function
hx(d){var
a=ak(GH,Gr,a8(d)),c=a[1];return 0===a[2]?c:b(e[3],Gi)}function
lQ(b,a){return A(function(b,c,a){return fe(fc(a),b)},a,b)}function
lR(e,c){return A(function(e,g,c){var
f=b(d[15],c);return a(d[38],e,f)},c,e)}function
lS(c){function
e(g){var
e=a(d[6],c,g),f=b(d[23],e);return a(d[9],f,c)}return function(a){return hu(e,a)}}function
lU(o,n){function
W(a){return[0,1,a]}var
X=[0,[0,1,n],a(f[17],W,o)];function
Y(b){function
c(a){return-a|0}var
d=a(f[17],c,b);return a(e[26],d,b)}var
Z=a(f[17],Y,X),l=b(f[1],o)+1|0,p=2*(b(f[1],n)+1|0)|0,h=(p+l|0)-1|0,_=dL([0,1,l],function(a){return E([0,p+a|0,a+1|0],GW)},q),$=cQ(1,Z,function(b,a){function
c(c,b){return E([0,b,a],[0,c])}var
d=1;return function(a){return cQ(d,b,c,a)}},_),U=Q(1,l);function
V(f){var
a=A(function(c,a,d){var
e=a[1];return a[2]===f?b(E(e,d),c):c},q,$);return[0,[0,h,h],A(function(d,a,c){return b(E([0,a,a],c),d)},q,a)]}var
i=a(f[17],V,U);if(a(d[26],lT,E8))var
m=fn(h);else
var
r=Q(1,h),s=function(a){return E(a,lT)},m=[0,h,g(f[21],s,r,q)];var
c=g(bD[14],0,GJ,GI),M=g(a9[4],c,0,cn(c)-6|0),j=a(e[17],M,GK),N=a(bD[4],cj,GL),t=b(f[1],i)-1|0,u=b(f[5],i)[1][1],v=Q(1,b(f[1],i));function
w(q,p,o){var
c=b(e[22],q-1|0),h=a(e[17],c,FC),d=0,i=p[2],j=dI(function(b,e,a){var
c=b[2],d=b[1];return c<d?a:[0,[0,[0,d,c],e],a]},i,d);function
k(a){return a[1]}var
l=aQ(function(a,b){return dH(k,a,b)},j);function
m(c,f){var
d=c[1],g=c[2],i=d[2],j=d[1],k=a(e[17],FE,f),l=dK(20,g),m=a(e[17],l,k),n=a(e[17],FF,m),o=b(e[22],i),p=a(e[17],o,n),q=a(e[17],FG,p),r=b(e[22],j),s=a(e[17],r,q);return a(e[17],h,s)}var
n=g(f[21],m,l,FD);return a(e[17],n,o)}var
x=C(f[26],w,v,i,FH),y=lK(m),z=a(e[17],y,x),B=a(e[17],FI,z),D=b(e[22],u),F=a(e[17],D,B),G=a(e[17],FJ,F),H=a(e[17],FK,G),I=b(e[22],t),J=a(e[17],I,H),K=a(e[17],FL,J),L=a(e[17],GM,K);ck(c,a(e[17],FM,L));ck(N,hy);var
O=a(e[17],j,GQ),P=a(e[17],GN,O),R=a(e[17],c,P),S=a(e[17],GO,R),T=a(e[17],cj,S),k=hF(a(e[17],GP,T));hx(fm(j));cS(c);cS(j);if(1!==k)if(2!==k)return 0===k?1:b(e[3],GV);return 0}function
lV(b){if(b){var
c=b[2],d=b[1];return lU(c,d)?c:a(e[26],c,[0,d,0])}throw[0,aV,GX]}function
GY(b,a){return hj(3,lV,[0,b,a])}function
hz(b,c){return a(d[26],b,GZ)?0:aH(function(c){return a(d[6],b,c)},c)}function
dN(c,b){function
e(b){return a(d[26],b,G0)}return aI(d[1],e,c,b)}function
lW(e,c){return A(function(h,g,f){var
c=b(fi(e),g),i=a(d[6],c,f);return a(d[1],h,i)},G1,c)}function
lX(k){return function(u){var
i=q,g=u;for(;;){if(g){var
m=g[2],c=g[1];if(aq(c)){var
g=m;continue}var
j=hn(c)[1];if(X(j,k))var
l=b(ho(j),c),h=aq(l)?b(e[3],G2):hn(l)[1];else
var
h=j;var
n=b(fi(c),h),p=b(ho(h),c),r=hz(a(d[9],G3,n),p),o=function(g,h,i){return function(c){var
e=a_(c,h,G4);if(a(d[26],e,G5))return c;var
f=b(d[3],e);return dN(c,hz(a(d[9],f,i),g))}}(c,h,n),s=a(f[17],o,m),t=aH(o,i),i=b(E(h,r),t),g=s;continue}var
v=0;return[0,cO(A(function(c,f,b){var
d=hm(fj(b),[0,k,0]);return a(e[26],d,c)},v,i)),i]}}}function
hA(c){var
g=c[1],f=g[1];if(g[2]!==f)return b(e[3],G7);function
i(l,g){var
c=l;for(;;){if(lC(g))return 0;var
h=bn(g,[0,c,c]);if(a(d[27],h,G8))return b(e[3],G9);if(a(d[26],h,G_)){if(lC(lF(c,g))){var
c=c+1|0;continue}return b(e[3],G$)}var
j=lF(c,g),k=hu(function(b){return a(d[9],b,h)},j);return[0,[0,h,k],i(c+1|0,[0,[0,f,f],dL([0,c+1|0,f],function(b){function
e(c){var
e=bn(k,c),f=bn(j,b),h=a(d[6],f,e),i=bn(g,[0,b,c]),l=a(d[4],i,h),m=[0,b,c];return function(a){return cR(m,l,a)}}var
h=[0,c+1|0,f];return function(a){return dL(h,e,a)}},q)])]}}return i(1,c)}function
lY(b){if(0===b)return[0,Ha,b];function
h(e){var
b=e[2],f=e[1],g=b[2],h=A(function(b,c,a){return fd(b,hi(a))},Hb,g),i=b[2],j=A(function(b,c,a){return fe(b,fc(a))},Hc,i),c=a(d[9],j,h),k=hu(function(b){return a(d[6],c,b)},b),l=a(d[6],c,c);return[0,a(d[9],f,l),k]}var
c=a(f[17],h,b);function
i(a){return a[1]}function
j(a){return aj(hi,i,a)}function
k(a){return aj(fd,j,a)}var
l=g(f[21],k,c,Hd);function
m(a){return a[1]}function
n(a){return aj(fc,m,a)}function
o(a){return aj(fe,n,a)}var
p=g(f[21],o,c,He),e=a(d[9],p,l);function
q(b){var
c=b[2];return[0,a(d[6],e,b[1]),c]}var
r=a(f[17],q,c);return[0,a(d[9],Hf,e),r]}function
hB(c,d){if(0<=c){if(0===c)return[0,q,0];if(0===d)return[0,q,0];var
g=Q(0,c),h=function(e){var
g=hB(c-e|0,b(f[6],d));function
h(a){return 0===e?a:b(E(b(f[5],d),e),a)}return a(f[17],h,g)},i=a(f[17],h,g);return bl(e[26],i)}return 0}function
hC(b,j){var
c=j;for(;;){if(0===b)return[0,[0,cl(bk),[5,bk]],0];if(0<=b){if(c){var
d=c[2],g=c[1],h=g[1],k=g[2],i=fq(h);if(0===i){var
c=d;continue}var
l=hC(b-i|0,d),m=function(a){var
b=[10,k,a[2]];return[0,bJ(h,a[1]),b]},n=a(f[17],m,l),o=hC(b,d);return a(e[26],o,n)}return[0,[0,cl(bk),[5,bk]],0]}return 0}}function
lZ(d,c,a){return A(function(a,e,d){return A(function(a,g,f){var
c=fo(e,g),h=a_(a,c,q);return b(E(c,dN(hz(d,f),h)),a)},a,c)},a,d)}function
HF(b){return a(d[26],b,HG)}var
HH=d[1];function
l0(b,c){return a(d[26],b,HI)?q:aH(function(c){return a(d[6],b,c)},c)}function
HK(aC,m,k,s,r){var
aD=0;function
aE(a){return a[1]}var
aF=a(f[17],aE,s),aG=a(e[26],[0,r,k],aF);function
aJ(a){return aj(hl,hw,a)}var
y=g(f[21],aJ,aG,aD);if(aC)var
aK=function(a){return fq(a[1])<=m?1:0},aL=a(f[35],aK,s),h=[0,[0,cl(bk),[5,bk]],aL];else
var
h=hC(m,s);var
aM=b(f[1],h);function
aN(e,d){var
c=hB(m-fq(d)|0,y),h=Q(1,b(f[1],c)),i=a(f[47],c,h);function
j(a){var
b=a[2],c=a[1];return E(c,a0([0,-e|0,-b|0,b],HL))}return[0,c,g(f[21],j,i,q)]}function
aO(h,e){var
c=hB((m-fq(e[1])|0)/2|0,y),i=Q(1,b(f[1],c)),d=a(f[47],c,i);function
j(e){var
c=e[2],g=e[1];function
i(e,a){var
d=e[2],f=fo(g,e[1]);if(d<c)return a;var
i=c===d?HM:HN,j=a_(a,f,q);return b(E(f,dN(a0([0,h,c,d],i),j)),a)}return a(f[21],i,d)}return[0,c,g(f[21],j,d,q)]}var
aP=Q(1,b(f[1],h)),aR=g(f[23],aO,aP,h),z=b(f[46],aR),B=z[1],aS=z[2],aT=Q(1,b(f[1],k)),aU=g(f[23],aN,aT,k),D=b(f[46],aU)[2],t=a(f[17],f[1],B),aV=dM(r),Z=A(function(e,c,a){return b(E(c,a0(Hg,b(d[3],a))),e)},q,aV);function
aW(c,b,a){return lZ(c[1],b,a)}var
aX=C(f[26],aW,h,aS,Z);function
aY(c,b,a){return lZ(c,b,a)}var
aZ=C(f[26],aY,k,D,aX),a1=0,a2=A(function(b,c,a){return[0,a,b]},a1,aZ),F=b(lX(HO),a2),c=F[1],a3=F[2],a4=[0,HP,c];function
a5(a){return E(a,a0(a,HQ))}var
u=g(f[21],a5,c,a3);function
a6(j){return A(function(e,c,k){var
h=c[3],i=c[2],f=c[1];if(0<=f){var
g=a_(k,j,HR);if(a(d[26],g,HS))return e;var
l=b(E([0,f,i,h],g),e);return b(E([0,f,h,i],g),l)}return e},q,u)}var
a7=A(function(b,a,c){var
d=a[3],e=a[2];if(0<a[1])if(e===d)return dN(c,b);return b},q,u),n=a(f[17],a6,a4),a8=cQ(1,c,function(b,a){var
c=a_(a7,b,HT);return function(b){return cR(a,c,b)}},q),G=[0,b(f[1],c),a8];if(0===c)var
H=fn(0);else{var
N=g(f[21],lQ,n,GR),O=lQ(G[2],GS),P=function(b){return a(d[6],N,b)},R=function(a){return aH(P,a)},w=a(f[17],R,n),x=lD(O,G),S=g(f[21],lR,w,GT),T=lR(x[2],GU),U=dG(20-(Math.log(b(d[56],S))/mT|0)|0),V=dG(20-(Math.log(b(d[56],T))/mT|0)|0),W=function(b){return a(d[6],b,U)},X=function(a){return aH(W,a)},o=a(f[17],X,w),Y=lD(V,x),i=g(bD[14],0,Hv,Hu),as=g(a9[4],i,0,cn(i)-6|0),p=a(e[17],as,Hw),at=a(bD[4],cj,Hx),_=b(f[1],o)-1|0,$=Q(1,b(f[1],o)),aa=function(p,o,n){var
c=b(e[22],p-1|0),h=a(e[17],c,Hh),d=0,i=A(function(b,a,e){var
c=a[3],d=a[2],f=a[1];return c<d?b:[0,[0,[0,f,d,c],e],b]},d,o);function
j(a){return a[1]}var
k=aQ(function(a,b){return dH(j,a,b)},i);function
l(d,f){var
c=d[1],g=d[2],i=c[3],j=c[2],k=c[1],l=a(e[17],Hj,f),m=dK(20,g),n=a(e[17],m,l),o=a(e[17],Hk,n),p=b(e[22],i),q=a(e[17],p,o),r=a(e[17],Hl,q),s=b(e[22],j),t=a(e[17],s,r),u=a(e[17],Hm,t),v=b(e[22],k),w=a(e[17],v,u);return a(e[17],h,w)}var
m=g(f[21],l,k,Hi);return a(e[17],m,n)},ab=C(f[26],aa,$,o,Hn),ac=lK(Y),ad=a(e[17],ac,ab),ae=a(e[17],Ho,ad),af=a(f[17],e[22],t),ag=a(a9[7],Hp,af),ah=a(e[17],ag,ae),ai=a(e[17],Hq,ah),ak=b(e[22],aM),al=a(e[17],ak,ai),am=a(e[17],Hr,al),an=b(e[22],_),ao=a(e[17],an,am),ap=a(e[17],Hs,ao),ar=a(e[17],Hy,ap);ck(i,a(e[17],Ht,ar));ck(at,hy);var
au=a(e[17],p,HC),av=a(e[17],Hz,au),aw=a(e[17],i,av),ax=a(e[17],HA,aw),ay=a(e[17],cj,ax),j=hF(a(e[17],HB,ay)),az=hx(fm(p));cS(i);cS(p);if(1===j)var
l=0;else
if(2===j)var
l=0;else
if(3===j)var
l=1;else
if(0===j)var
l=1;else{var
aA=b(e[22],j),aB=a(e[17],HE,aA);b(e[3],aB);var
l=1}if(!l)b(e[3],HD);var
H=az}function
I(i){var
c=b(lS(i),H),l=l0(HJ,a(f[7],n,0));function
j(b,d){var
e=a(f[7],n,b);return aI(HH,HF,l0(bn(c,b),e),d)}var
k=dL([0,1,c[1]],j,l),d=Q(1,b(f[1],t)),e=a(f[47],t,d);function
g(a){var
c=a[1],d=a[2];return[0,[0,c,c],A(function(c,a,e){var
f=a[3],g=a[2];return a[1]===d?b(E([0,g,f],e),c):c},q,k)]}var
h=a(f[17],g,e);return[0,c,a(f[17],hA,h)]}if(0===c)var
v=I(bk);else
var
br=Q(5,66),bs=a(f[17],dG,br),bt=Q(1,31),bu=a(f[17],d[47],bt),v=hk(I,a(e[26],bu,bs));var
J=v[1],a$=v[2],ba=a0(HV,HU),bb=Q(1,J[1]);function
bc(b){var
d=bn(J,b);return E(a(f[7],c,b-1|0),d)}var
K=g(f[21],bc,bb,ba),bd=A(function(d,c,a){return b(E(c,lW(K,a)),d)},K,u);function
be(a){return A(function(c,b,a){return cR(b,lW(bd,a),c)},q,a)}function
bf(c){function
d(d){var
e=d[2],h=d[1],i=Q(1,b(f[1],c));function
j(b,d){var
g=bn(e,b);return cR(a(f[7],c,b-1|0),g,d)}return[0,h,g(f[21],j,i,q)]}return b(f[17],d)}var
bg=g(f[23],bf,B,a$),L=a(f[17],be,D);function
bh(b,a){return[0,b,a]}var
bi=g(f[23],bh,h,bg);function
bj(a){return 0!==a[2]?1:0}var
M=a(f[35],bj,bi),bl=dM(r);function
bm(b,a){var
c=bJ(b,a);return function(a){return cm(c,a)}}var
bo=C(f[26],bm,L,k,bl);function
bp(a){var
c=a[2],d=a[1][1];function
b(a){var
b=a[2],c=a[1],d=lH(c,bJ(b,b));return function(a){return cm(d,a)}}var
e=bJ(d,g(f[21],b,c,q));return function(a){return cm(e,a)}}if(aq(g(f[21],bp,M,bo))){var
bq=function(a){return[0,a[1][2],a[2]]};return[0,L,a(f[17],bq,M)]}throw hs}function
hD(a){var
b=ch(a);function
c(a){return a[1]}return aQ(function(a,b){return dH(c,a,b)},b)}function
l1(a){if(X(a,q))return[0,bk];var
b=hD(a),c=0;function
d(a,d){var
b=a[2],c=a[1],e=1===b?[1,c]:[6,[0,[1,c],b]];return[0,e,d]}var
e=g(f[21],d,b,c);return bl(function(b,a){return[5,[0,b,a]]},e)}function
HW(e){var
b=e[2],c=e[1];return X(c,q)?[0,b]:a(d[26],b,bk)?l1(c):[5,[0,[0,b],l1(c)]]}function
l2(b){if(X(b,q))return 0;var
c=ch(b),d=aQ(function(C,B){var
o=B[1],p=C[1];if(X(o,q))return 1;if(X(p,q))return 0;var
k=hD(p),l=hD(o),t=0;function
u(a){return a[2]}function
v(b,a){return b+a|0}function
w(a){return aj(v,u,a)}var
m=g(f[21],w,k,t),x=0;function
y(a){return a[2]}function
z(b,a){return b+a|0}function
A(a){return aj(z,y,a)}var
n=g(f[21],A,l,x);if(m<n)return 0;if(n<m)return 1;var
b=k,a=l;for(;;){if(b){if(a){var
c=a[1],d=c[2],e=c[1],h=b[1],i=h[2],j=h[1],r=a[2],s=b[2];if(bZ(j,e))return 1;if(bZ(e,j))return 0;if(bZ(i,d))return 0;if(bZ(d,i))return 1;var
b=s,a=r;continue}return 0}return a?1:1}},c),e=a(f[17],HW,d);return bl(function(b,a){return[3,[0,b,a]]},e)}function
HX(a){var
b=a[1];return[10,[5,b],[6,l2(a[2])]]}function
HY(b){var
c=b[2],d=b[1];if(0===c)return d;var
e=a(f[17],HX,c);return[10,d,bl(function(b,a){return[9,b,a]},e)]}function
l3(b){if(0===b)return HZ;var
c=0;function
d(c,d){var
g=l3(hm(b,[0,c,0]));function
h(a){return[0,c,a]}var
i=a(f[17],h,g);return a(e[26],i,d)}return g(f[21],d,b,c)}function
hE(d,c){return A(function(g,e,c){return b(E(a(f[38],e,d),c),g)},q,c)}aA(1050,[0,Fe,dM,bJ,fp,cl,bK,l2,HY,Fq,HK,function(c){var
n=hw(c),o=hw(c),K=fj(c);function
L(b){function
c(a){return lG(a,b)}return a(f[17],c,o)}var
u=a(f[17],L,K);function
M(f){var
b=0;return(A(function(c,b,g){var
d=lG(f,b);return a(e[6],d,c)},b,c)+1|0)/2|0}var
N=a(f[17],M,o);function
O(a){var
b=Q(0,a);function
c(b,a){return[0,b,a]}return function(a){return ff(c,b,a)}}var
P=g(f[21],O,N,G6),H=[0,b(f[5],u),0],I=b(f[6],u),t=g(f[21],GY,I,H),J=hj(b(f[1],t),lV,t);function
R(b){function
c(a){return 2*a|0}return lU(J,a(f[17],c,b))}var
S=a(f[35],R,P),T=b(f[9],S);function
U(a){function
c(d,c,a){return 0===c?a:b(E(d,c),a)}return C(f[26],c,o,a,q)}var
i=a(f[17],U,T),r=b(f[1],i),aD=l3(n);function
aE(d){var
e=a(f[47],n,d);return aq(hv(c,A(function(d,c,a){return b(E(hE(e,c),a),d)},q,c)))}var
aF=a(f[35],aE,aD),aG=Q(1,b(f[1],i)),v=a(f[47],i,aG),aJ=ff(function(b,a){return[0,[0,b[1],a[1]],[0,b[2],a[2]]]},v,v);function
aK(b){var
a=b[2];return a[1]<=a[2]?1:0}var
aL=a(f[35],aK,aJ);function
aM(b){var
c=b[2],d=b[1],e=c[2],g=c[1],h=d[2],i=d[1];function
j(b){var
c=hE(a(f[47],n,b),h);return[0,[0,hE(a(f[47],n,b),i),c],[0,g,e]]}return a(f[17],j,aF)}var
aN=a(f[17],aM,aL),w=bl(e[26],aN);function
aO(a){return a[1]}var
aP=cO(a(f[17],aO,w));function
aR(b){function
c(a){return X(a[1],b)}return a(f[35],c,w)}var
aS=a(f[17],aR,aP);function
aT(a){return a[2]}var
aU=b(f[17],aT);function
aV(a){return aj(cO,aU,a)}var
aW=a(f[17],aV,aS);function
aX(c,d){if(c){var
g=c[2],h=c[1];if(g){var
i=function(a){var
c=a0(h,Im);return b(E(a,In),c)},j=a(f[17],i,g);return a(e[26],j,d)}return d}throw hs}var
aY=g(f[21],aX,aW,0),aZ=A(function(d,c,a){return b(E(c,a0(Io,a)),d)},q,c),a1=cQ(1,i,function(f,a){function
c(g,d,c){var
e=fo(f,g);if(d<a)return c;var
h=a===d?Ip:Iq,i=a_(c,e,q);return b(E(e,b(E([0,a,d],h),i)),c)}var
d=1;return function(a){return cQ(d,i,c,a)}},aZ),a2=0,a3=A(function(b,c,a){return[0,a,b]},a2,a1),a4=a(e[26],a3,aY),x=b(lX(Ir),a4),j=x[1],a5=x[2];function
a6(a){return E(a,a0(a,Is))}var
y=g(f[21],a6,j,a5),a7=[0,It,j],a8=Q(1,r);function
a$(a){return b(fi(y),[0,a,a])}var
ba=bl(dN,a(f[17],a$,a8));function
bb(i){return[0,[0,r,r],A(function(f,e,j){var
g=e[2],h=e[1],c=a_(j,i,Iu);if(a(d[26],c,Iv))return f;var
k=b(E([0,h,g],c),f);return b(E([0,g,h],c),k)},q,y)]}var
h=a(f[17],bb,a7),bc=cQ(1,j,function(b,a){var
c=a_(ba,b,Iw);return function(b){return cR(a,c,b)}},q),z=[0,b(f[1],j),bc];if(0===j)var
D=fn(0);else{var
k=g(bD[14],0,Ic,Ib),at=g(a9[4],k,0,cn(k)-6|0),p=a(e[17],at,Id),au=a(bD[4],cj,Ie),ac=b(f[1],h)-1|0,ad=b(f[5],h)[1][1],ae=Q(1,b(f[1],h)),af=function(q,p,o){var
c=b(e[22],q-1|0),h=a(e[17],c,H2),d=0,i=p[2],j=dI(function(b,e,a){var
c=b[2],d=b[1];return c<d?a:[0,[0,[0,d,c],e],a]},i,d);function
k(a){return a[1]}var
l=aQ(function(a,b){return dH(k,a,b)},j);function
m(c,f){var
d=c[1],g=c[2],i=d[2],j=d[1],k=a(e[17],H4,f),l=dK(20,g),m=a(e[17],l,k),n=a(e[17],H5,m),o=b(e[22],i),p=a(e[17],o,n),q=a(e[17],H6,p),r=b(e[22],j),s=a(e[17],r,q);return a(e[17],h,s)}var
n=g(f[21],m,l,H3);return a(e[17],n,o)},ag=C(f[26],af,ae,h,H7),V=Q(1,z[1]),W=function(a){return bn(z,a)},Y=20,Z=function(a){return dK(Y,a)},_=function(a){return aj(Z,W,a)},$=a(f[17],_,V),aa=a(a9[7],H1,$),ab=a(e[17],aa,H0),ah=a(e[17],ab,ag),ai=a(e[17],H8,ah),ak=b(e[22],ad),al=a(e[17],ak,ai),am=a(e[17],H9,al),an=a(e[17],H_,am),ao=b(e[22],ac),ap=a(e[17],ao,an),ar=a(e[17],H$,ap),as=a(e[17],If,ar);ck(k,a(e[17],Ia,as));ck(au,hy);var
av=a(e[17],p,Ij),aw=a(e[17],Ig,av),ax=a(e[17],k,aw),ay=a(e[17],Ih,ax),az=a(e[17],cj,ay),l=hF(a(e[17],Ii,az)),aA=hx(fm(p));cS(k);cS(p);if(1===l)var
m=0;else
if(2===l)var
m=0;else
if(3===l)var
m=1;else
if(0===l)var
m=1;else{var
aB=b(e[22],l),aC=a(e[17],Il,aB);b(e[3],aC);var
m=1}if(!m)b(e[3],Ik);var
D=aA}function
bd(c){var
l=b(lS(c),D),g=lE(a(f[7],h,0));function
i(n,m){var
o=a(f[7],h,n),p=bn(l,n),g=o[1],i=g[2],j=g[1];if(a(d[26],p,E$))var
c=[0,[0,j,i],q];else
var
r=o[2],c=[0,[0,j,i],aH(function(b){return a(d[6],p,b)},r)];var
k=c[1];if(B.caml_notequal(k,m[1]))return b(e[3],Fa);var
s=m[2],t=c[2];function
u(b){return a(d[26],b,Fb)}return[0,k,aI(d[1],u,t,s)]}return lY(hA(dL([0,1,l[1]],i,g)))}if(0===j)var
s=lY(hA(lE(a(f[7],h,0))));else
var
bh=Q(5,66),bi=a(f[17],dG,bh),bj=Q(1,31),bk=a(f[17],d[47],bj),s=hk(bd,a(e[26],bk,bi));var
F=s[1],be=s[2];function
bf(c){var
d=c[1],e=c[2][2];return[0,d,A(function(e,d,c){return b(E(a(f[7],i,d-1|0),c),e)},q,e)]}var
G=a(f[17],bf,be);function
bg(a){var
b=a[1],c=fp(a[2],2);return bJ(cl(b),c)}if(aq(hv(lH(F,bl(cm,a(f[17],bg,G))),c)))return[0,F,G];throw hs}],"Micromega_plugin__Sos");return}
