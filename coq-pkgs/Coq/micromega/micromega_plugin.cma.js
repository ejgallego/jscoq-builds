function(J3){"use strict";var
nN="QMicromega",n7="_vendor+v8.11+32bit/coq/plugins/micromega/vect.ml",oa="*",ob=" - ",jz="param.csdp",n6="__varmap",n$="diagonalize: not PSD",nI="csdp: error ",ox="monoid",oF="__p",om=",",jI='"',oE=" 1 ",ol="1\n",oD="__ff",nM="=",n5="find_pivot",ok="%s*%a",ow="Require Import ZArith Lia. Open Scope Z_scope.\n",jq='"\n',nT="buggy certificate",n_="lia",ap="ZMicromega",oj=" * ",N="micromega",he='command "',n4=")^2",nH="compare_num",jJ="real_nonlinear_prover",jx=" [*] ",oi="__wit",ou="Nia",ov=143,nL="Zero",hh="Lia",jy="; csdp ",nS="ERROR: no compatible proof",aS="",ct="RingMicromega",oh='Unfortunately Coq isn\'t aware of the presence of any "csdp" executable in the path. \n\n',jG="real nonlinear prover",jF="0",jE="> /dev/null",n3="%a * %a",nG="__arith",ev="Reals",oC=", False\n",jw=438,bc=248,nE="<=",nF="QArith",jD="cd ",n9=") * (",dv=" + ",B="Coq",ot="psatz_R",os=".",nD=" ; ",nQ="Csdp packages are provided by some OS distributions; binaries and source code can be downloaded from https://projects.coin-or.org/Csdp",nR=" : ",jp="_vendor+v8.11+32bit/coq/plugins/micromega/mfourier.ml",f=246,nP="-%a",n8="(%a) * (%a)",oB="x%i",jv="Cache",nK=" = ",og=118,af="Tauto",jC="%a + %a",of=119,oA=108,aq=104,jH=" Cannot find witness",oe="csdp: Problem is infeasible",or="Z",n2="the use of a specialized external tool called csdp. \n\n",ax=" ",nC=" >= 0 ; ",oz="e",ju=120,oq="EnvRing",hg="_vendor+v8.11+32bit/coq/plugins/micromega/certificate.ml",nB="t",nJ="PsatzZ",jt=".dat-s",n1="-",oy="Rdefinitions",n0="Timeout",od="D",oo="linear prover",op="Depth",js=" := ",nO="(%a)^2",oc=" Skipping what remains of this tactic: the complexity of the goal requires ",nY="psatz_Q",nZ='" exited ',hf="%s",nX="Not implemented",nV="psatz_Z",jB=".out",nW="Rpow_def",nU="nat",nA="%i",jA="sos",jr="pure_sos",aR="\n",on=0.693147180559945286,aG="VarMap",k=250,E=J3.jsoo_runtime,J=E.caml_check_bound,hb=E.caml_compare,ab=E.caml_equal,bb=E.caml_fresh_oo_id,dt=E.caml_int_compare,ny=E.caml_int_of_string,cs=E.caml_lessthan,J0=E.caml_list_of_js_array,cX=E.caml_ml_string_length,hc=E.caml_mul,c=E.caml_new_string,j=E.caml_obj_tag,aF=E.caml_register_global,nw=E.caml_string_equal,bZ=E.caml_string_get,du=E.caml_sys_remove,jo=E.caml_sys_system_command,q=E.caml_wrap_exception;function
b(a,b){return a.length==1?a(b):E.caml_call_gen(a,[b])}function
a(a,b,c){return a.length==2?a(b,c):E.caml_call_gen(a,[b,c])}function
h(a,b,c,d){return a.length==3?a(b,c,d):E.caml_call_gen(a,[b,c,d])}function
F(a,b,c,d,e){return a.length==4?a(b,c,d,e):E.caml_call_gen(a,[b,c,d,e])}function
U(a,b,c,d,e,f){return a.length==5?a(b,c,d,e,f):E.caml_call_gen(a,[b,c,d,e,f])}function
V(a,b,c,d,e,f,g){return a.length==6?a(b,c,d,e,f,g):E.caml_call_gen(a,[b,c,d,e,f,g])}function
hd(a,b,c,d,e,f,g,h){return a.length==7?a(b,c,d,e,f,g,h):E.caml_call_gen(a,[b,c,d,e,f,g,h])}function
nz(a,b,c,d,e,f,g,h,i){return a.length==8?a(b,c,d,e,f,g,h,i):E.caml_call_gen(a,[b,c,d,e,f,g,h,i])}function
J2(a,b,c,d,e,f,g,h,i,j){return a.length==9?a(b,c,d,e,f,g,h,i,j):E.caml_call_gen(a,[b,c,d,e,f,g,h,i,j])}function
nx(a,b,c,d,e,f,g,h,i,j,k,l){return a.length==11?a(b,c,d,e,f,g,h,i,j,k,l):E.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l])}function
J1(a,b,c,d,e,f,g,h,i,j,k,l,m){return a.length==12?a(b,c,d,e,f,g,h,i,j,k,l,m):E.caml_call_gen(a,[b,c,d,e,f,g,h,i,j,k,l,m])}var
r=E.caml_get_global_data(),bo=[0,0,0],hY=[0,0],h7=[0,0],bs=[0,1],k0=c(" \t\n\r"),k1=c(",;"),k2=c("()[]{}"),k3=c("\\!@#$%^&*-+|\\<=>/?~.:"),k4=c("'abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ"),k5=c("0123456789"),ln=[0,1],io=c("axtol=1.0e-8\natytol=1.0e-8\nobjtol=1.0e-8\npinftol=1.0e8\ndinftol=1.0e8\nmaxiter=100\nminstepfrac=0.9\nmaxstepfrac=0.97\nminstepp=1.0e-8\nminstepd=1.0e-8\nusexzgap=1\ntweakgap=0\naffine=0\nprintlevel=1\n"),JZ=[4,3,0,0,[11,c(nC),[4,3,0,0,[11,c(js),[4,3,0,0,[11,c(nC),[15,0]]]]]]],mI=J0([[0,c(B),[0,c("Lists"),[0,c("List"),0]]],[0,c(B),[0,c(N),[0,c(ap),0]]],[0,c(B),[0,c(N),[0,c(af),0]]],[0,c(B),[0,c(N),[0,c("DeclConstant"),0]]],[0,c(B),[0,c(N),[0,c(ct),0]]],[0,c(B),[0,c(N),[0,c(oq),0]]],[0,c(B),[0,c(N),[0,c(ap),0]]],[0,c(B),[0,c(N),[0,c("RMicromega"),0]]],[0,c(B),[0,c(N),[0,c(af),0]]],[0,c(B),[0,c(N),[0,c(ct),0]]],[0,c(B),[0,c(N),[0,c(oq),0]]],[0,c(B),[0,c(nF),[0,c("QArith_base"),0]]],[0,c(B),[0,c(ev),[0,c(oy),0]]],[0,c(B),[0,c(ev),[0,c(nW),0]]],[0,c("LRing_normalise"),0]]),aQ=c("micromega_plugin"),e=r.Stdlib,K=r.Not_found,O=r.Unix,bi=r.Stdlib__hashtbl,cE=r.Hashset,n=r.Stdlib__printf,eY=r.Stdlib__marshal,eT=r.Stdlib__printexc,g=r.Stdlib__list,o=r.Big_int,aV=r.Assert_failure,d=r.Num,dQ=r.Ratio,hT=r.Stdlib__set,cC=r.Stdlib__map,fb=r.Failure,aW=r.Stdlib__string,k7=r.End_of_file,bN=r.Stdlib__filename,l2=r.Invalid_argument,i=r.CamlinternalLazy,fR=r.CErrors,m=r.Util,aO=r.Option,fY=r.CList,ca=r.Names,l=r.EConstr,_=r.Tacticals,aC=r.Tacmach,ba=r.Tactics,cV=r.Pp,cW=r.Proofview,nc=r.Environ,mM=r.Stdlib__array,m4=r.Context,mK=r.Retyping,mW=r.Evd,ee=r.Coqlib,b$=r.Goptions,aD=r.Ltac_plugin__Tacinterp,a2=r.Ltac_plugin__Tacentries,aE=r.Ltac_plugin__Tacarg,ai=r.Genarg,jn=r.Stdarg,t1=r.Sys_error,Bd=r.CMap,HV=r.Coq_config,HZ=r.Envars,HJ=r.CAst,Hm=r.Evarutil,Hn=r.Refine,GL=r.Sorts,Gu=r.Redexpr,Gf=r.UnivProblem,Gg=r.Univ,Ga=r.Reductionops,FW=r.Typeclasses,Dg=r.UnivGen,HU=r.System,IM=r.Mltop;aF(988,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"Micromega_plugin");var
p6=[0,0],qh=[0,0,0],qi=[0,[0,0],0],qn=[0,[0,0],0],qo=[0,0,0],ql=[0,[0,0],0],qm=[0,0,0],qa=[0,[0,0],0],qb=[0,0,0],p_=[0,[0,0],0],p$=[0,0,0],p5=[0,0],pZ=[0,0],pY=[0,0],pX=[0,0],pU=[0,[1,0]],pV=[0,[0,0]],pW=[0,[0,0]],pQ=[0,[1,0]],pR=[0,[0,0]],pS=[0,0],pT=[0,[1,0]],pt=[0,[0,0],0],ps=[0,0,0],pp=[0,1],pq=[0,2],pr=[0,3],pk=[0,0],pm=[0,0],pl=[0,1],po=[0,3],pn=[0,0],pj=[0,0,0],o1=[0,[1,0]],o2=[0,0,[0,0]],o3=[0,[0,0],0],o4=[0,0],o5=[0,[1,0]],o6=[0,[1,0]],o7=[0,0],o8=[0,[1,0]],o9=[0,[1,0]],o_=[0,[1,0]],o$=[0,0],pa=[0,[1,0]],pb=[0,0,0],pc=[0,0,0],pd=[0,0],pe=[0,0,0],pf=[0,0],oV=[0,0],oT=[0,0],oS=[1,0],oR=[0,0],oI=[1,0],oJ=[1,0],oK=[0,0],oM=[0,0],oL=[0,0],py=[0,0],pK=[0,0],p3=[0,0],p8=[0,[0,0],0],p9=[0,0,0],qc=[0,0,0],qd=[0,0,0],qe=[0,[0,0],0],qf=[0,0,0],qj=[0,[0,0],0],qk=[0,0,0],qp=[0,0,0],qq=[0,0,0],sm=[0,[11,c(he),[2,0,[11,c(nZ),[4,3,0,0,0]]]],c('command "%s" exited %i')],sl=[0,[11,c(he),[2,0,[11,c(nZ),[2,0,0]]]],c('command "%s" exited %s')],sn=[0,[11,c(he),[2,0,[11,c('" killed '),[4,3,0,0,0]]]],c('command "%s" killed %i')],so=[0,[11,c(he),[2,0,[11,c('" stopped '),[4,3,0,0,0]]]],c('command "%s" stopped %i')],rN=[0,c("_vendor+v8.11+32bit/coq/plugins/micromega/mutils.ml"),226,19],rK=[0,0,0],rJ=[0,0,0],rG=[0,0,0],q4=[0,[4,3,0,0,[12,32,0]],c("%i ")],sG=[0,[15,[11,c(dv),[15,0]]],c(jC)],sH=c(jF),sO=c(nH),sP=c(nH),sS=[0,0],sT=[0,0],s1=[0,0],s2=[0,0],s4=[0,c(n7),208,11],s5=[0,1],td=[0,0],tc=[0,1],s9=[0,0],s8=[0,c(n7),236,8],s3=[0,[0,0,[0,0]],0],sZ=[0,0],sY=[0,0],sX=[0,0],sU=[0,1],sV=[0,1],sR=[0,1],sQ=[0,0],sN=[0,0],sM=[0,0],sK=[0,[15,[12,32,0]],c("%a ")],sL=[0,[11,c("(+ "),[15,[12,41,0]]],c("(+ %a)")],sI=[0,[12,ju,[4,3,0,0,0]],c(oB)],sC=[0,0],sD=[0,[2,0,0],c(hf)],sF=[0,[11,c("(- "),[15,[12,41,0]]],c("(- %a)")],sE=[0,[11,c("(* "),[2,0,[12,32,[15,[12,41,0]]]]],c("(* %s %a)")],sy=[0,0],sz=[0,[2,0,0],c(hf)],sB=[0,[12,45,[15,0]],c(nP)],sA=[0,[2,0,[12,42,[15,0]]],c(ok)],te=c(jF),tf=[0,[12,og,[2,0,0]],c("v%s")],tg=[0,[11,c("- ("),[15,[12,41,0]]],c("- (%a)")],th=[0,[12,40,[15,[11,c(")+("),[15,[12,41,0]]]]],c("(%a)+(%a)")],ti=[0,[12,40,[15,[11,c(")-("),[15,[12,41,0]]]]],c("(%a)-(%a)")],tj=[0,[12,40,[15,[11,c(")*("),[15,[12,41,0]]]]],c("(%a)*(%a)")],tk=[0,[12,40,[15,[11,c(")^("),[4,3,0,0,[12,41,0]]]]],c("(%a)^(%i)")],tl=[0,[11,c("Aeq("),[4,3,0,0,[12,41,0]]],c("Aeq(%i)")],tm=[0,[11,c("Ale("),[4,3,0,0,[12,41,0]]],c("Ale(%i)")],tn=[0,[11,c("Alt("),[4,3,0,0,[12,41,0]]],c("Alt(%i)")],to=[0,[11,c("eq("),[2,0,[12,41,0]]],c("eq(%s)")],tp=[0,[11,c("le("),[2,0,[12,41,0]]],c("le(%s)")],tq=[0,[11,c("lt("),[2,0,[12,41,0]]],c("lt(%s)")],tr=[0,[12,40,[15,[11,c(n4),0]]],c(nO)],ts=[0,[11,c(ox),0],c(ox)],tt=[0,[15,[11,c(oj),[15,0]]],c(n3)],tu=[0,[15,[11,c(dv),[15,0]]],c(jC)],tv=[0,[15,[11,c(oj),[15,0]]],c(n3)],tB=c("end_itlist"),tD=c("tryfind"),tF=c("choose: completely undefined function"),t3=c(aR),t2=c("strings_of_file: can't open "),tY=c(" expected"),tG=c("apply"),tC=c(aS),tx=[0,2],ty=[0,10],tH=c("Micromega_plugin.Sos_lib.Noparse"),t5=c("Micromega_plugin.Sos_lib.TooDeep"),uy=[0,1],xb=[0,0,0],xK=[0,0],xI=[0,0],xJ=[0,0],xG=[0,1],xD=[0,1],xE=[0,2],xC=[0,0,0],xA=[0,1],xB=[0,-1],xF=[0,0,0],xH=[0,0,0],xy=c(oe),xz=c(nI),xp=c(jt),xq=c(jA),xr=c(jB),xs=c(jz),xt=c(aS),xx=c(jE),xu=c(ax),xv=c(jy),xw=c(jD),xj=c(aS),xk=c(aR),xl=c(ol),xm=c(aR),xn=c(jq),xo=c(jI),xg=c(aR),xh=c(ax),xi=c(ax),xe=c(oE),xf=c(aS),xc=c(aR),xd=c(ax),w7=[0,0],w5=[0,0],w6=[0,0],w4=[0,1],w0=[0,1],w1=[0,2],wZ=[0,1],w2=[0,0,0,0],w3=[0,0,0,0],w8=[0,-1],w9=[0,0,0,0],wW=[0,0],wU=[0,0],wR=c(oe),wS=c(nI),wI=c(jt),wJ=c(jA),wK=c(jB),wL=c(jz),wM=c(aS),wQ=c(jE),wN=c(ax),wO=c(jy),wP=c(jD),wB=c(aS),wC=c(aR),wD=c(ax),wE=c(aR),wF=c(aR),wG=c(jq),wH=c(jI),wx=c(aR),wy=c(ax),wz=c(ax),wA=c(ax),wv=c(ax),ww=c(aS),wu=[0,0,0,0],wp=[0,0],wq=[0,1],wo=[0,0],wr=[0,0],ws=[0,1],wt=[0,1],wk=[0,0],wl=c(n$),wm=[0,0],wn=c(n$),wj=c("diagonalize: non-square matrix"),wi=[0,0,0],wg=[0,0],wh=[0,0],wf=[0,-1],we=c("choose_variable"),wd=[0,0],wc=[0,0],wb=[0,0],v$=[0,c("_vendor+v8.11+32bit/coq/plugins/micromega/sos.ml"),535,12],v_=[0,1],v9=c("linear_program: An error occurred in the SDP solver"),v5=[0,1],v6=[0,1],v7=[0,0],v8=[0,0],vW=c(jt),vX=c(jA),vY=c(jB),vZ=c(jz),v0=c(aS),v4=c(jE),v1=c(ax),v2=c(jy),v3=c(jD),vB=[0,0,0],vw=c("mkparser: unparsed input"),vs=[0,10],vg=c("+"),vh=c(n1),uV=c(aS),uW=c(aR),uX=c(ol),uY=c(aR),uZ=c(jq),u0=c(jI),uS=c(aR),uT=c(ax),uU=c(ax),uQ=c(oE),uR=c(aS),uO=c(aR),uP=c(ax),uH=[0,0],uI=c(ob),uJ=c(dv),uF=c("<<0>>"),uG=c(aS),uK=c(">>"),uL=c(dv),uN=c(n1),uM=c("<<"),uC=[0,1],uD=c(oa),uA=c("1"),uB=c(oa),uz=c("^"),ux=[0,0],uw=[0,0],uv=[0,0],uu=[0,0],ut=[0,1],up=[0,0],uo=c("matrix_add: incompatible dimensions"),un=[0,0],ul=[0,0],uk=[0,0],uj=[0,0],ui=[0,0],t7=[0,10],t8=[0,1],t9=[0,10],t_=[0,1],t$=[0,10],ua=[0,0],ub=c("0.0"),uc=[0,1],ud=c(aS),uh=c(oz),ue=[0,0],uf=c("-0."),ug=c("0."),t6=c("Micromega_plugin.Sos.Sanity"),u$=c(os),vk=c("E"),vm=c(oz),vx=c("}"),vy=c(om),vz=c("{"),vC=c(nM),vD=c("xVec"),vG=c(aR),vI=c(ax),vO=c(ax),wX=[0,-1],zX=c("pivot: equality as second argument"),zY=[0,1],zZ=[0,-1],zW=[0,1,0,0],zS=[0,0],zT=[0,-1],zU=c("cutting_plane ignore strict constraints"),zP=[0,0],zQ=[0,[11,c("mult_error "),[15,[11,c(jx),[15,[12,10,0]]]]],c("mult_error %a [*] %a\n")],zK=[0,[15,[12,10,0]],c("%a\n")],zI=[0,[15,[12,32,[2,0,[11,c(" 0 by "),[15,[12,10,0]]]]]],c("%a %s 0 by %a\n")],y5=[0,[11,c(nL),0],c(nL)],y6=[0,[12,40,[15,[12,41,[12,64,[2,0,0]]]]],c("(%a)@%s")],y7=[0,[11,c("Hyp "),[4,3,0,0,0]],c("Hyp %i")],y8=[0,[11,c("Def "),[4,3,0,0,0]],c("Def %i")],y9=[0,[11,c("Cst "),[2,0,0]],c("Cst %s")],y_=[0,[12,40,[15,[11,c(n4),0]]],c(nO)],y$=[0,[12,40,[15,[11,c(n9),[15,[12,41,0]]]]],c(n8)],za=[0,[12,40,[15,[11,c(")/"),[2,0,0]]]],c("(%a)/%s")],zb=[0,[12,40,[15,[11,c(n9),[15,[12,41,0]]]]],c(n8)],zc=[0,[15,[11,c(dv),[15,0]]],c(jC)],zd=[0,[12,91,[15,[12,93,0]]],c("[%a]")],ze=[0,[12,46,0],c(os)],zf=[0,[4,3,0,0,[11,c(":= "),[15,[11,c(nD),[15,0]]]]],c("%i:= %a ; %a")],zg=c(";"),zh=[0,[4,3,0,0,[12,123,[15,[11,c(nE),[15,[11,c(nE),[15,[12,125,[15,0]]]]]]]]],c("%i{%a<=%a<=%a}%a")],zi=[0,[4,3,0,0,[11,c(js),[4,3,0,0,[11,c(nK),[4,3,0,0,[11,c(ob),[4,3,0,0,[11,c(nD),[4,3,0,0,[11,c(js),JZ]]]]]]]]]],c("%i := %i = %i - %i ; %i := %i >= 0 ; %i := %i >= 0 ; %a")],zk=[0,1],zl=[0,1],zj=[0,0],zm=[0,c("_vendor+v8.11+32bit/coq/plugins/micromega/polynomial.ml"),678,13],zn=[0,1],zq=[0,1],zp=[0,1],zo=[0,1],zx=[0,0],zy=c("eval_prf_rule : negative constant"),zz=[0,[11,c("MulC("),[15,[12,44,[15,[11,c(") invalid 2d arg "),[15,[12,32,[2,0,0]]]]]]]],c("MulC(%a,%a) invalid 2d arg %a %s")],zA=c("eval_prf_rule : not an equality"),zD=c("Proof is not finished"),zE=[0,[11,c("Last inference "),[15,[12,32,[2,0,[12,10,0]]]]],c("Last inference %a %s\n")],zF=c(nX),zG=c(nX),zB=[0,0],zv=c("Cuts should already be compiled"),zs=[0,[4,3,0,0,[12,44,[2,0,0]]],c("%i,%s")],zt=c(aS),zu=[0,[11,c("id_of_hyp "),[4,3,0,0,[12,32,[2,0,0]]]],c("id_of_hyp %i %s")],y2=[0,[11,c("(H"),[4,3,0,0,[11,c(nR),[15,[12,32,[2,0,[11,c(" 0)\n"),0]]]]]]],c("(H%i : %a %s 0)\n")],y0=[0,[11,c("(x"),[4,3,0,0,[11,c(nR),[2,0,[11,c(") "),0]]]]],c("(x%i : %s) ")],y1=[0,[11,c("forall "),[15,[12,10,0]]],c("forall %a\n")],y3=[0,[11,c(oC),0],c(oC)],yV=[0,0],yS=[0,[12,og,[4,3,0,0,0]],c("v%i")],yQ=[0,1],yR=[0,0],yP=[0,0],yO=[0,1],yM=[0,1],yJ=[0,[11,c("Cannot reserve "),[4,3,0,0,0]],c("Cannot reserve %i")],yH=[0,[15,[12,32,[2,0,[12,32,[2,0,0]]]]],c("%a %s %s")],yD=c(nM),yE=c(">="),yF=c(">"),yA=[0,0],yB=[0,0],yy=[0,0],yx=[0,1],yv=[0,0],yu=[0,[15,[11,c(dv),0]],c("%a + ")],yq=[0,0],yr=[0,[2,0,0],c(hf)],yt=[0,[12,45,[15,0]],c(nP)],ys=[0,[2,0,[12,42,[15,0]]],c(ok)],xQ=[0,[15,[12,42,[15,0]]],c("%a*%a")],xO=[0,[12,ju,[4,3,0,0,0]],c(oB)],xP=[0,[12,ju,[4,3,0,0,[12,94,[4,3,0,0,0]]]],c("x%i^%i")],yC=c("Micromega_plugin.Polynomial.Strict"),zL=c("Micromega_plugin.Polynomial.WithProof.InvalidProof"),z6=[0,0],z7=c("find_pivot_column"),AG=[0,0],AF=[0,1],AC=[0,1],AD=[0,0],Az=[0,1],AA=[0,1],AB=[0,1],Au=[0,0],Av=[0,2],Aw=[0,1],Ax=[0,1],Ay=[0,1],AE=[0,1],As=[0,1],Ar=[0,-1],Am=[0,0],An=c("push_real"),Ao=[0,-1],Ap=[0,0],Aq=[0,1],Al=[0,0],Ak=c(n5),Ah=[0,0],Ag=c("pivot"),Ad=[0,0],Ae=[0,0],Af=[0,1],z_=[0,0],z$=c("Cannot solve column"),Aa=[0,-1],Ab=[0,0],Ac=[0,-1],z9=c(n5),z8=[0,0],z4=[0,0],z2=[0,[11,c("Cannot restrict "),[4,3,0,0,0]],c("Cannot restrict %i")],AN=[0,0,0],AL=[0,0,0],AJ=[0,0,[0,5,0]],AM=[0,1,[0,4,[0,5,0]]],AK=[0,1,[0,6,[0,5,0]]],AH=c("Micromega_plugin.Persistent_cache.PHashtable(Key).InvalidTableFormat"),AI=c("Micromega_plugin.Persistent_cache.PHashtable(Key).UnboundTable"),AT=[0,1],AO=[0,[12,91,[2,0,0]],c("[%s")],AS=c("]-oo"),AP=c(om),AQ=[0,[2,0,[12,93,0]],c("%s]")],AR=c("+oo["),AV=[0,[12,72,[4,3,0,0,0]],c("H%i")],AW=[0,[11,c("E("),[4,3,0,0,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("E(%i,%a,%a)")],AX=[0,[11,c("A("),[15,[12,44,[15,[12,41,0]]]]],c("A(%a,%a)")],BJ=[0,1],BK=[0,[0,0,0]],BG=c("merge_proof : pivot is not possible"),BH=[0,1],BI=[0,1],BF=[0,0],Bz=[0,1],BA=[0,1],BB=[0,1],BC=[0,1],BD=[0,1],BE=[0,1],Bv=[0,0],Bw=[0,-1],Bx=[0,[11,c("optimise Exception : "),[2,0,0]],c("optimise Exception : %s")],Bl=[0,0],Bm=[0,0],Bn=[0,0],Bo=[0,0],Bp=[0,0],Bq=[0,0],Br=[0,0],Bs=[0,0],Bi=[0,[11,c("bound_of_variable: eval_vecr "),[15,[11,c(nK),[2,0,[12,44,[15,[12,10,0]]]]]]],c("bound_of_variable: eval_vecr %a = %s,%a\n")],Bj=[0,[11,c("current interval:  "),[15,[12,10,0]]],c("current interval:  %a\n")],Bk=c("bound_of_variable: impossible"),Bh=[0,0,0],Bf=[0,0,0],Bg=c("SystemContradiction"),Be=[0,0],Bb=[0,0],Ba=[0,0,0,0],A6=[0,0],A7=[0,0],A_=[0,c(jp),191,2],A8=[0,1],A9=[0,1],A4=[0,c(jp),ov,6],A3=[0,0,0],A2=[0,0],AZ=[0,c(jp),81,2],AU=c("Micromega_plugin.Mfourier.SystemContradiction"),A1=c("Micromega_plugin.Mfourier.TimeOut"),B$=c("scale term: not implemented"),Ca=[0,0],Ce=[0,0],CF=c("nia"),CE=c(n_),Cw=c(".v"),Cx=[0,[11,c(ow),0],c(ow)],Cy=c(or),Cz=[0,[11,c("Goal "),[15,[11,c(".\n"),0]]],c("Goal %a.\n")],CB=[0,[11,c("Proof.\n intros. "),[2,0,[11,c(".\nQed.\n"),0]]],c("Proof.\n intros. %s.\nQed.\n")],CA=[0,[11,c("Proof.\n intros. Fail "),[2,0,[11,c(".\nAbort.\n"),0]]],c("Proof.\n intros. Fail %s.\nAbort.\n")],Cr=[0,c(hg),896,4],Cs=[0,0],Ct=[0,1],Cu=[0,c(hg),927,4],Cn=[0,1],Co=[0,0,0],Cp=c("Interval without proof"),Ck=[0,1],Cl=[0,-1],Cf=[0,0],Cg=[0,1],Cd=[0,0],Cb=[0,1],Cc=[0,0],B_=c("P"),B9=[0,1],B6=[0,1],B7=[0,-1],B2=[0,1],B3=[0,c(hg),316,6],B4=c("check_sat : Unexpected operator"),B5=[0,0],B0=[0,c(hg),283,11],BX=[0,0],BY=c("dual_raw_certificate: empty_certificate"),BU=[0,1],BT=[0,0],BN=[0,0],BQ=[0,[0,0],0],BR=[0,0,0],B1=c("Micromega_plugin.Certificate.FoundProof"),G3=[0,[12,68,0],c(od)],G4=[0,[11,c("R["),[15,[12,44,[15,[12,93,0]]]]],c("R[%a,%a]")],G5=[0,[11,c("C["),[15,[12,44,[15,[12,93,0]]]]],c("C[%a,%a]")],G6=c("]"),G7=c("["),G8=[0,[11,c("EP["),[15,[12,44,[15,[12,44,[15,[12,93,0]]]]]]],c("EP[%a,%a,%a]")],G9=[0,[11,c("Ex["),[15,[12,44,[15,[12,93,0]]]]],c("Ex[%a,%a]")],Hh=c("abstract_wrt_formula"),IA=c(jJ),Iz=c(jJ),Iy=c(jJ),Ir=c(jG),Iq=c(jG),Ip=c(jG),H4=c(nT),H3=c(nT),HW=c("csdpcert"),HX=c(N),HY=c("plugins"),HK=c(nG),HL=[0,0],HI=c(jH),HD=c(n0),HE=c(nQ),HF=c(oh),HG=c(n2),HH=c(oc),Hx=c(oi),Hy=c(nB),Hz=[0,[0,c(B),[0,c(N),[0,c(aG),0]]],[0,[0,c(aG),0],0]],HA=c(aG),HB=c(n6),HC=c(oD),Ht=c(jH),Hu=c(nG),Hv=c(jH),Ho=c(n0),Hp=c(nQ),Hq=c(oh),Hr=c(n2),Hs=c(oc),Hl=[0,1],Hf=[0,[11,c(nS),0],c(nS)],Hg=c("Cannot find compatible proof"),Hd=c("bad old index"),He=c("proof compaction error"),Hc=[0,0],G$=c(oi),Ha=c(n6),Hb=c(oD),G2=[0,[15,[12,47,[15,0]]],c("%a/%a")],GZ=c(nB),G0=[0,[0,c(B),[0,c(N),[0,c(aG),0]]],[0,[0,c(aG),0],0]],G1=c(aG),GW=c("Empty"),GX=[0,[0,c(B),[0,c(N),[0,c(aG),0]]],[0,[0,c(aG),0],0]],GY=c(aG),GT=c("Elt"),GU=[0,[0,c(B),[0,c(N),[0,c(aG),0]]],[0,[0,c(aG),0],0]],GV=c(aG),GQ=c("Branch"),GR=[0,[0,c(B),[0,c(N),[0,c(aG),0]]],[0,[0,c(aG),0],0]],GS=c(aG),Gw=[0,0],GP=[0,[11,c(oF),[4,3,0,0,0]],c("__p%i")],GO=c(oF),GN=c("__x"),GM=[0,c("_vendor+v8.11+32bit/coq/plugins/micromega/coq_micromega.ml"),1254,13],GF=c("error : parse_arith(2)"),GB=[0,0,0],Gh=c("get_rank"),Ge=[1,c("Oups")],F5=[0,[12,48,0],c(jF)],F6=[0,[11,c("(In "),[15,[12,41,[12,37,[11,c(nU),0]]]]],c("(In %a)%%nat")],F7=[0,[12,40,[15,[11,c("^2)"),0]]],c("(%a^2)")],F8=[0,[11,c("( "),[15,[11,c(jx),[15,[12,41,0]]]]],c("( %a [*] %a)")],F9=[0,[12,40,[15,[11,c(jx),[15,[12,41,0]]]]],c("(%a [*] %a)")],F_=[0,[12,40,[15,[11,c(" [+] "),[15,[12,41,0]]]]],c("(%a [+] %a)")],F$=[0,[12,40,[15,[12,41,[12,37,[11,c("positive"),0]]]]],c("(%a)%%positive")],F2=[0,[11,c("Pc "),[15,0]],c("Pc %a")],F3=[0,[11,c("Pinj("),[15,[12,44,[15,[12,41,0]]]]],c("Pinj(%a,%a)")],F4=[0,[11,c("PX("),[15,[12,44,[15,[12,44,[15,[12,41,0]]]]]]],c("PX(%a,%a,%a)")],FZ=[0,[15,[11,c(" ,"),[15,0]]],c("%a ,%a")],F0=[0,[15,0],c("%a")],F1=[0,[2,0,[15,[2,0,0]]],c("%s%a%s")],FX=[0,[2,0,0],c(hf)],FV=[0,[4,3,0,0,0],c(nA)],FU=[0,[4,3,0,0,0],c(nA)],FP=c("Formula"),FQ=[0,[0,c(B),[0,c(N),[0,c(ct),0]]],[0,[0,c(ct),0],0]],FR=c(ct),FM=c("Build_Formula"),FN=[0,[0,c(B),[0,c(N),[0,c(ct),0]]],[0,[0,c(ct),0],0]],FO=c(ct),FJ=c("QWitness"),FK=[0,[0,c(B),[0,c(N),[0,c(nN),0]]],0],FL=c(nN),FG=c("BFormula"),FH=[0,[0,c(B),[0,c(N),[0,c(af),0]]],[0,[0,c(af),0],0]],FI=c(ap),FC=c("I"),FD=[0,[0,c(B),[0,c(N),[0,c(af),0]]],[0,[0,c(af),0],0]],FE=c(ap),Fy=c("X"),Fz=[0,[0,c(B),[0,c(N),[0,c(af),0]]],[0,[0,c(af),0],0]],FA=c(ap),Fu=c("A"),Fv=[0,[0,c(B),[0,c(N),[0,c(af),0]]],[0,[0,c(af),0],0]],Fw=c(ap),Fq=c("N"),Fr=[0,[0,c(B),[0,c(N),[0,c(af),0]]],[0,[0,c(af),0],0]],Fs=c(ap),Fm=c(od),Fn=[0,[0,c(B),[0,c(N),[0,c(af),0]]],[0,[0,c(af),0],0]],Fo=c(ap),Fi=c("Cj"),Fj=[0,[0,c(B),[0,c(N),[0,c(af),0]]],[0,[0,c(af),0],0]],Fk=c(ap),Fe=c("FF"),Ff=[0,[0,c(B),[0,c(N),[0,c(af),0]]],[0,[0,c(af),0],0]],Fg=c(ap),Fa=c("TT"),Fb=[0,[0,c(B),[0,c(N),[0,c(af),0]]],[0,[0,c(af),0],0]],Fc=c(ap),E$=c("DeclaredConstant"),E_=c(nJ),E9=c("PsatzC"),E8=c("PsatzAdd"),E7=c("PsatzMulC"),E6=c("PsatzMulE"),E5=c("PsatzSquare"),E4=c("PsatzIn"),E3=c("OpGt"),E2=c("OpGe"),E1=c("OpLt"),E0=c("OpLe"),EZ=c("OpNEq"),EY=c("OpEq"),EX=c("Pinj"),EW=c("Pc"),EV=c("PX"),EU=c("PEpow"),ET=c("PEsub"),ES=c("PEmul"),ER=c("PEopp"),EQ=c("PEadd"),EP=c("PEc"),EO=c("PEX"),EN=c("Q2R"),EM=c("IZR"),EL=c("powerRZ"),EK=c("pow"),EJ=c("Rinv"),EI=c("Rmult"),EH=c("Ropp"),EG=c("Rminus"),EF=c("Rplus"),ED=c("Rlt"),EB=c("Rle"),Ez=c("Rge"),Ex=c("Rgt"),Ew=c("Qpower"),Ev=c("Qmult"),Eu=c("Qopp"),Et=c("Qminus"),Es=c("Qplus"),Eq=c("Qeq"),Eo=c("Qlt"),Em=c("Qle"),El=c("Z.pow"),Ek=c("Z.mul"),Ej=c("Z.opp"),Ei=c("Z.sub"),Eh=c("Z.add"),Eg=c("eq"),Ee=c("Z.lt"),Ec=c("Z.le"),Ea=c("Z.ge"),D_=c("Z.gt"),D9=c("ExProof"),D8=c("EnumProof"),D7=c("CutProof"),D6=c("RatProof"),D5=c("DoneProof"),D4=c("ZArithProof"),D3=c("R1"),D2=c("R0"),D1=c("COpp"),D0=c("CInv"),DZ=c("CPow"),DY=c("CMult"),DX=c("CMinus"),DW=c("CPlus"),DV=c("CZ"),DU=c("CQ"),DT=c("C1"),DS=c("C0"),DR=c("Rcst"),DQ=c("Qmake"),DP=c("R"),DO=c("Q"),DN=c("Zneg"),DM=c("Zpos"),DL=c("Z0"),DK=c(or),DJ=c("xI"),DI=c("xO"),DH=c("xH"),DG=c("Npos"),DF=c("N0"),DE=c("inr"),DD=c("inl"),DC=c("tt"),DB=c("None"),DA=c("unit"),Dz=c(nU),Dy=c("S"),Dx=c("O"),Dw=c("list"),Dv=c("nil"),Du=c("cons"),Dt=c("False"),Ds=c("True"),Dr=c("iff"),Dq=c("not"),Dp=c("or"),Do=c("and"),CH=c(aS),CJ=[0,c(hh),[0,c("Enum"),0]],CK=c("Lia Enum"),CN=[0,c("Simplex"),0],CO=c("Use the Simplex instead of Fourier elimination"),CR=[0,c("Dump"),[0,c("Arith"),0]],CS=c("Generate Coq goals in file from calls to 'lia' 'nia'"),CV=[0,c(hh),[0,c(jv),0]],CW=c("cache of lia (.lia.cache)"),CZ=[0,c(ou),[0,c(jv),0]],C0=c("cache of nia (.nia.cache)"),C3=[0,c("Nra"),[0,c(jv),0]],C4=c("cache of nra (.nra.cache)"),C6=[0,c("Lra"),[0,c(op),0]],C8=[0,c(hh),[0,c(op),0]],C_=[0,c(B),[0,c("Logic"),[0,c("Decidable"),0]]],Dd=[0,[0,c(B),[0,c("Numbers"),[0,c("BinNums"),0]]],0],De=[0,[0,c(B),[0,c(ev),[0,c(oy),0]]],[0,[0,c(B),[0,c(ev),[0,c(nW),0]]],[0,[0,c(B),[0,c(ev),[0,c("Raxioms"),0]]],[0,[0,c(B),[0,c(nF),[0,c("Qreals"),0]]],0]]]],Df=[0,[0,c(B),[0,c("ZArith"),[0,c("BinInt"),0]]],0],Di=c(ap),Dj=c(ap),Dk=c(ap),Dl=c(ap),Dm=c(ap),Dn=c(ap),FS=c("Micromega_plugin.Coq_micromega.M.ParseError"),Hi=c("Micromega_plugin.Coq_micromega.CsdpNotFound"),HT=c("csdp"),H1=c(".csdp.cache"),H9=c(".lia.cache"),Ia=c(".nia.cache"),Id=c(".nra.cache"),Ih=c(oo),Il=c(oo),Io=c("nra"),Is=c(n_),Iu=c("nlia"),IB=c(jr),IE=c(jr),IH=c(jr),IO=[0,c("myred"),0],IQ=c("RED"),IT=c(nV),IX=c(nV),IZ=c(nJ),I2=c("xlia"),I4=c(hh),I7=c("xnlia"),I9=c(ou),Ja=c("xnra"),Jc=c("NRA"),Jf=c("xnqa"),Jh=c("NQA"),Jk=c("sos_Z"),Jm=c("Sos_Z"),Jp=c("sos_Q"),Jr=c("Sos_Q"),Ju=c("sos_R"),Jw=c("Sos_R"),Jz=c("lra_Q"),JB=c("LRA_Q"),JE=c("lra_R"),JG=c("LRA_R"),JJ=c(ot),JN=c(ot),JP=c("PsatzR"),JS=c(nY),JW=c(nY),JY=c("PsatzQ");function
bd(a){return 0===a?1:0}function
b0(a){return a[1]}function
oG(a){return a[2]}function
dw(a,b){if(a){var
c=a[1];return[0,c,dw(a[2],b)]}return b}function
jK(a){switch(a){case
0:return 0;case
1:return 2;default:return 1}}function
hi(b,a){return b?[0,hi(b[1],a)]:a}function
jL(e,d,c){var
b=e,a=d;for(;;){if(b){var
f=b[1];if(a){var
b=f,a=a[2];continue}return c}return a?a[1]:c}}function
ar(d,c){var
a=d,b=c;for(;;){if(a){var
e=[0,a[1],b],a=a[2],b=e;continue}return b}}function
cY(c,a){if(a){var
d=a[1],e=cY(c,a[2]);return[0,b(c,d),e]}return 0}function
ew(f,e,d){var
b=e,c=d;for(;;){if(b){var
g=b[2],h=a(f,c,b[1]),b=g,c=h;continue}return c}}function
ex(d,c,b){if(b){var
e=b[1];return a(d,e,ex(d,c,b[2]))}return c}var
oH=[0];function
bG(a){return typeof
a==="number"?oI:0===a[0]?[1,bG(a[1])]:[0,a[1]]}function
cZ(b,a){if(typeof
b==="number")return typeof
a==="number"?oJ:0===a[0]?[1,bG(a[1])]:[0,a[1]];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[1,bG(c)]:0===a[0]?[1,dx(c,a[1])]:[0,cZ(c,a[1])]}var
d=b[1];return typeof
a==="number"?[0,d]:0===a[0]?[0,cZ(d,a[1])]:[1,cZ(d,a[1])]}}function
dx(b,a){if(typeof
b==="number")return typeof
a==="number"?oK:0===a[0]?[0,bG(a[1])]:[1,bG(a[1])];else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,bG(c)]:0===a[0]?[0,dx(c,a[1])]:[1,dx(c,a[1])]}var
d=b[1];return typeof
a==="number"?[1,bG(d)]:0===a[0]?[1,dx(d,a[1])]:[0,cZ(d,a[1])]}}function
dy(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[0,dy(a[1])]}function
dz(a){return typeof
a==="number"?0===a?oL:1:[0,[0,a[1]]]}function
dA(a){return typeof
a==="number"?a:[0,[1,a[1]]]}function
jM(a){return typeof
a==="number"?0:0===a[0]?[0,[1,[1,a[1]]]]:[0,[1,dy(a[1])]]}function
c0(b,a){if(typeof
b==="number")return typeof
a==="number"?0:1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,[1,c]]:0===a[0]?dA(c0(c,a[1])):dz(c0(c,a[1]))}var
d=b[1];return typeof
a==="number"?[0,dy(d)]:0===a[0]?dz(dB(d,a[1])):dA(c0(d,a[1]))}}function
dB(b,a){if(typeof
b==="number")return 1;else{if(0===b[0]){var
c=b[1];return typeof
a==="number"?[0,dy(c)]:0===a[0]?dz(dB(c,a[1])):dA(c0(c,a[1]))}var
d=b[1];return typeof
a==="number"?jM(d):0===a[0]?dA(dB(d,a[1])):dz(dB(d,a[1]))}}function
hj(c,b){var
a=c0(c,b);return typeof
a==="number"?0:a[1]}function
hk(b,a){return typeof
b==="number"?a:0===b[0]?cZ(a,[1,hk(b[1],a)]):[1,hk(b[1],a)]}function
ey(a,h,g){var
d=h,c=g;for(;;)if(typeof
c==="number")return b(a,d);else{if(0===c[0]){var
e=c[1];return b(a,ey(a,ey(a,d,e),e))}var
f=c[1],d=ey(a,d,f),c=f;continue}}function
dC(a){return typeof
a==="number"?oM:0===a[0]?[0,dC(a[1])]:[0,dC(a[1])]}function
jN(h,g,f){var
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
oN=0;function
ez(a,b){return jN(oN,a,b)}function
oO(b,a){return 2<=ez(b,a)?b:a}function
oP(b,a){return 2<=ez(b,a)?0:1}function
hl(j,i,h){var
c=j,b=i,a=h;for(;;){if(c){var
d=c[1];if(typeof
b==="number")return 0;else{if(0===b[0]){var
e=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
f=a[1];switch(ez(e,f)){case
0:return b;case
1:var
c=d,a=b,b=hj(f,e);continue;default:var
c=d,b=hj(e,f);continue}}var
c=d,a=a[1];continue}}var
g=b[1];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=d,b=g;continue}return[1,hl(d,g,a[1])]}}}return 0}}function
oQ(b,a){var
c=dC(a);return hl(hi(dC(b),c),b,a)}function
jO(a){return a?bG(jO(a[1])):0}var
w=[0,bG,cZ,dx,dy,dz,dA,jM,c0,dB,hj,hk,ey,dC,jN,ez,oO,oP,hl,oQ,jO],jP=[0,function(a){return a?[0,b(w[20],a[1])]:0}];function
eA(b,d,c){if(typeof
c==="number")return d;else{if(0===c[0]){var
e=eA(b,d,c[1]);return a(b,d,a(b,e,e))}var
f=eA(b,d,c[1]);return a(b,f,f)}}function
hm(a){return typeof
a==="number"?0:0===a[0]?[0,[1,a[1]]]:[1,[1,a[1]]]}function
jQ(a){return typeof
a==="number"?oR:0===a[0]?[0,[0,a[1]]]:[1,b(w[4],a[1])]}function
jR(a){return typeof
a==="number"?oS:0===a[0]?[0,b(w[4],a[1])]:[1,[0,a[1]]]}function
cu(c,a){if(typeof
c==="number")return typeof
a==="number"?0:0===a[0]?[1,[1,a[1]]]:[1,b(w[4],a[1])];else{if(0===c[0]){var
d=c[1];return typeof
a==="number"?[0,[1,d]]:0===a[0]?hm(cu(d,a[1])):jQ(cu(d,a[1]))}var
e=c[1];return typeof
a==="number"?[0,b(w[4],e)]:0===a[0]?jR(cu(e,a[1])):hm(cu(e,a[1]))}}function
b1(c,b){if(typeof
c==="number")return b;else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?c:0===b[0]?[0,a(w[2],d,b[1])]:cu(d,b[1])}var
e=c[1];return typeof
b==="number"?c:0===b[0]?cu(b[1],e):[1,a(w[2],e,b[1])]}}function
cv(a){return typeof
a==="number"?0:0===a[0]?[1,a[1]]:[0,a[1]]}function
eB(b,a){return b1(b,cv(a))}function
b2(c,b){if(typeof
c==="number")return 0;else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?0:0===b[0]?[0,a(w[11],d,b[1])]:[1,a(w[11],d,b[1])]}var
e=c[1];return typeof
b==="number"?0:0===b[0]?[1,a(w[11],e,b[1])]:[0,a(w[11],e,b[1])]}}function
jS(b){function
c(a){return b2(b,a)}return a(w[12],c,oT)}function
oU(c,a){if(typeof
a==="number")return oV;else{if(0===a[0]){var
d=a[1];return b(jS(c),d)}return 0}}function
dD(c,b){if(typeof
c==="number")return typeof
b==="number"?0:0===b[0]?1:2;else{if(0===c[0]){var
d=c[1];if(typeof
b!=="number"&&0===b[0])return a(w[15],d,b[1]);return 2}var
e=c[1];if(typeof
b!=="number"&&1===b[0])return jK(a(w[15],e,b[1]));return 1}}function
jT(b,a){return 2<=dD(b,a)?0:1}function
hn(b,a){return 1===dD(b,a)?1:0}function
oW(b,a){return 2<=dD(b,a)?1:0}function
oX(b,a){return 1===dD(b,a)?a:b}function
eC(a){if(typeof
a!=="number"&&1===a[0])return[0,a[1]];return a}function
oY(a){if(typeof
a!=="number"&&0===a[0])return[0,a[1]];return 0}function
oZ(a){return a?[0,b(w[20],a[1])]:0}function
o0(a){return a?[0,a[1]]:0}function
cw(b,a){if(typeof
b==="number")return jT(o1,a)?o2:o3;else{if(0===b[0]){var
e=cw(b[1],a),f=e[1],c=b1(b2(o5,e[2]),o4);if(hn(c,a))return[0,b2(o6,f),c];var
i=eB(c,a);return[0,b1(b2(o8,f),o7),i]}var
g=cw(b[1],a),h=g[1],d=b2(o9,g[2]);if(hn(d,a))return[0,b2(o_,h),d];var
j=eB(d,a);return[0,b1(b2(pa,h),o$),j]}}function
jU(b,a){if(typeof
b==="number")return pb;else{if(0===b[0]){var
c=b[1];if(typeof
a==="number")return pc;else{if(0===a[0])return cw(c,a);var
d=cw(c,[0,a[1]]),e=d[2],f=d[1];if(typeof
e==="number")return[0,cv(f),0];var
l=b1(a,e);return[0,cv(b1(f,pd)),l]}}var
g=b[1];if(typeof
a==="number")return pe;else{if(0===a[0]){var
h=cw(g,a),i=h[2],j=h[1];if(typeof
i==="number")return[0,cv(j),0];var
m=eB(a,i);return[0,cv(b1(j,pf)),m]}var
k=cw(g,[0,a[1]]),n=k[1];return[0,n,cv(k[2])]}}}function
pg(b,a){return jU(b,a)[1]}var
p=[0,hm,jQ,jR,cu,b1,cv,eB,b2,jS,oU,dD,jT,hn,oW,oX,eC,oY,oZ,o0,cw,jU,pg,function(c,b){if(typeof
c==="number")return eC(b);else{if(0===c[0]){var
d=c[1];return typeof
b==="number"?eC(c):0===b[0]?[0,a(w[19],d,b[1])]:[0,a(w[19],d,b[1])]}var
e=c[1];return typeof
b==="number"?eC(c):0===b[0]?[0,a(w[19],e,b[1])]:[0,a(w[19],e,b[1])]}}];function
aT(c,b){return 0===a(p[11],c,b)?1:0}function
ph(a){return[0,a]}function
pi(a){return[0,a]}function
ho(d,f,e){var
c=f,b=e;for(;;)switch(c[0]){case
0:var
g=c[1];return 0===b[0]?a(d,g,b[1]):0;case
1:var
h=c[2],i=c[1];if(1===b[0]){var
j=b[2];if(0===a(w[15],i,b[1])){var
c=h,b=j;continue}return 0}return 0;default:var
k=c[3],l=c[2],m=c[1];if(2===b[0]){var
n=b[3],o=b[1];if(0===a(w[15],l,b[2])){if(ho(d,m,o)){var
c=k,b=n;continue}return 0}return 0}return 0}}function
aj(c,b){switch(b[0]){case
0:return b;case
1:var
d=b[2];return[1,a(w[2],c,b[1]),d];default:return[1,c,b]}}function
jV(a,c){return typeof
a==="number"?c:0===a[0]?[1,[1,a[1]],c]:[1,b(w[4],a[1]),c]}function
W(f,e,b,d,c){switch(b[0]){case
0:return a(e,b[1],f)?aj(0,c):[2,b,d,c];case
1:return[2,b,d,c];default:var
g=b[2],h=b[1];return ho(e,b[3],[0,f])?[2,h,a(w[2],g,d),c]:[2,b,d,c]}}function
jW(c,b,a){return[2,[0,b],a,[0,c]]}function
jX(b,a){return jW(b,a,0)}function
ac(c,a){switch(a[0]){case
0:return[0,b(c,a[1])];case
1:var
d=a[1];return[1,d,ac(c,a[2])];default:var
e=a[2],f=a[1],g=ac(c,a[3]);return[2,ac(c,f),e,g]}}function
b3(d,b,c){switch(b[0]){case
0:return[0,a(d,b[1],c)];case
1:var
e=b[1];return[1,e,b3(d,b[2],c)];default:var
f=b[2],g=b[1];return[2,g,f,b3(d,b[3],c)]}}function
c1(d,b,c){switch(b[0]){case
0:return[0,a(d,b[1],c)];case
1:var
e=b[1];return[1,e,c1(d,b[2],c)];default:var
f=b[2],g=b[1];return[2,g,f,c1(d,b[3],c)]}}function
dE(g,f,e,c,d){switch(d[0]){case
0:return aj(c,b3(g,e,d[1]));case
1:var
i=d[2],m=d[1],h=a(p[4],m,c);return typeof
h==="number"?aj(c,a(f,i,e)):0===h[0]?aj(c,a(f,[1,h[1],i],e)):aj(m,dE(g,f,e,h[1],i));default:var
j=d[3],k=d[2],l=d[1];return typeof
c==="number"?[2,l,k,a(f,j,e)]:0===c[0]?[2,l,k,dE(g,f,e,[1,c[1]],j)]:[2,l,k,dE(g,f,e,b(w[4],c[1]),j)]}}function
dF(h,g,f,e,c,d){switch(d[0]){case
0:var
o=d[1];return aj(c,b3(h,ac(g,e),o));case
1:var
j=d[2],n=d[1],i=a(p[4],n,c);return typeof
i==="number"?aj(c,a(f,j,e)):0===i[0]?aj(c,a(f,[1,i[1],j],e)):aj(n,dF(h,g,f,e,i[1],j));default:var
k=d[3],l=d[2],m=d[1];return typeof
c==="number"?[2,m,l,a(f,k,e)]:0===c[0]?[2,m,l,dF(h,g,f,e,[1,c[1]],k)]:[2,m,l,dF(h,g,f,e,b(w[4],c[1]),k)]}}function
hp(f,g,j,d,e,c){switch(c[0]){case
0:return[2,d,e,c];case
1:var
k=c[2],h=c[1];return typeof
h==="number"?[2,d,e,k]:0===h[0]?[2,d,e,[1,[1,h[1]],k]]:[2,d,e,[1,b(w[4],h[1]),k]];default:var
l=c[3],m=c[2],n=c[1],i=a(p[4],m,e);return typeof
i==="number"?W(f,g,a(j,n,d),m,l):0===i[0]?W(f,g,a(j,[2,n,i[1],[0,f]],d),e,l):W(f,g,hp(f,g,j,d,i[1],n),m,l)}}function
hq(g,f,h,k,d,e,c){switch(c[0]){case
0:return[2,ac(f,d),e,c];case
1:var
l=c[2],i=c[1];if(typeof
i==="number")return[2,ac(f,d),e,l];else{if(0===i[0]){var
q=[1,[1,i[1]],l];return[2,ac(f,d),e,q]}var
r=[1,b(w[4],i[1]),l];return[2,ac(f,d),e,r]}default:var
m=c[3],n=c[2],o=c[1],j=a(p[4],n,e);return typeof
j==="number"?W(g,h,a(k,o,d),n,m):0===j[0]?W(g,h,a(k,[2,o,j[1],[0,g]],d),e,m):W(g,h,hq(g,f,h,k,d,j[1],o),n,m)}}function
ag(c,e,d,f,g){switch(g[0]){case
0:return b3(e,f,g[1]);case
1:var
r=g[2],s=g[1];return dE(e,function(a,b){return ag(c,e,d,a,b)},r,s,f);default:var
h=g[3],j=g[2],i=g[1];switch(f[0]){case
0:return[2,i,j,b3(e,h,f[1])];case
1:var
m=f[2],k=f[1];return typeof
k==="number"?[2,i,j,ag(c,e,d,m,h)]:0===k[0]?[2,i,j,ag(c,e,d,[1,[1,k[1]],m],h)]:[2,i,j,ag(c,e,d,[1,b(w[4],k[1]),m],h)];default:var
n=f[3],o=f[2],q=f[1],l=a(p[4],o,j);if(typeof
l==="number"){var
t=ag(c,e,d,n,h);return W(c,d,ag(c,e,d,q,i),o,t)}else{if(0===l[0]){var
u=l[1],v=ag(c,e,d,n,h);return W(c,d,ag(c,e,d,[2,q,u,[0,c]],i),j,v)}var
x=l[1],y=ag(c,e,d,n,h);return W(c,d,hp(c,d,function(a,b){return ag(c,e,d,a,b)},i,x,q),o,y)}}}}function
$(d,f,g,c,e,h,i){switch(i[0]){case
0:return c1(g,h,i[1]);case
1:var
t=i[2],u=i[1];return dF(f,c,function(a,b){return $(d,f,g,c,e,a,b)},t,u,h);default:var
j=i[3],l=i[2],k=i[1];switch(h[0]){case
0:var
v=h[1],x=b3(f,ac(c,j),v);return[2,ac(c,k),l,x];case
1:var
o=h[2],m=h[1];if(typeof
m==="number"){var
y=$(d,f,g,c,e,o,j);return[2,ac(c,k),l,y]}else{if(0===m[0]){var
z=$(d,f,g,c,e,[1,[1,m[1]],o],j);return[2,ac(c,k),l,z]}var
A=$(d,f,g,c,e,[1,b(w[4],m[1]),o],j);return[2,ac(c,k),l,A]}default:var
q=h[3],r=h[2],s=h[1],n=a(p[4],r,l);if(typeof
n==="number"){var
B=$(d,f,g,c,e,q,j);return W(d,e,$(d,f,g,c,e,s,k),r,B)}else{if(0===n[0]){var
C=n[1],D=$(d,f,g,c,e,q,j);return W(d,e,$(d,f,g,c,e,[2,s,C,[0,d]],k),l,D)}var
E=n[1],F=$(d,f,g,c,e,q,j);return W(d,e,hq(d,c,e,function(a,b){return $(d,f,g,c,e,a,b)},k,E,s),r,F)}}}}function
dG(f,e,d,b,c){switch(b[0]){case
0:return[0,a(e,b[1],c)];case
1:var
g=b[1];return aj(g,dG(f,e,d,b[2],c));default:var
h=b[2],i=b[1],j=dG(f,e,d,b[3],c);return W(f,d,dG(f,e,d,i,c),h,j)}}function
dH(d,g,f,c,e,b){return a(c,b,d)?[0,d]:a(c,b,g)?e:dG(d,f,c,e,b)}function
bH(f,j,i,e,g,d,c,h){switch(h[0]){case
0:return aj(c,dH(f,j,i,e,d,h[1]));case
1:var
l=h[2],q=h[1],k=a(p[4],q,c);return typeof
k==="number"?aj(c,a(g,l,d)):0===k[0]?aj(c,a(g,[1,k[1],l],d)):aj(q,bH(f,j,i,e,g,d,k[1],l));default:var
m=h[3],n=h[2],o=h[1];if(typeof
c==="number"){var
r=a(g,m,d);return W(f,e,bH(f,j,i,e,g,d,0,o),n,r)}else{if(0===c[0]){var
s=bH(f,j,i,e,g,d,[1,c[1]],m);return W(f,e,bH(f,j,i,e,g,d,c,o),n,s)}var
t=bH(f,j,i,e,g,d,b(w[4],c[1]),m);return W(f,e,bH(f,j,i,e,g,d,c,o),n,t)}}}function
as(a,e,f,d,c,g,h){switch(h[0]){case
0:return dH(a,e,d,c,g,h[1]);case
1:var
q=h[2],r=h[1];return bH(a,e,d,c,function(b,g){return as(a,e,f,d,c,b,g)},q,r,g);default:var
i=h[3],m=h[2],k=h[1];switch(g[0]){case
0:return dH(a,e,d,c,h,g[1]);case
1:var
l=g[2],j=g[1],s=typeof
j==="number"?as(a,e,f,d,c,l,i):0===j[0]?as(a,e,f,d,c,[1,[1,j[1]],l],i):as(a,e,f,d,c,[1,b(w[4],j[1]),l],i);return W(a,c,as(a,e,f,d,c,g,k),m,s);default:var
n=g[3],o=g[2],p=g[1],t=as(a,e,f,d,c,n,i),u=0,v=bH(a,e,d,c,function(b,g){return as(a,e,f,d,c,b,g)},i,u,p),x=as(a,e,f,d,c,aj(0,n),k),y=as(a,e,f,d,c,p,k),z=W(a,c,v,o,t);return ag(a,f,c,W(a,c,ag(a,f,c,W(a,c,y,o,[0,a]),x),m,[0,a]),z)}}}function
dI(b,e,g,f,c,d){switch(d[0]){case
0:var
h=d[1];return[0,a(f,h,h)];case
1:var
l=d[1];return[1,l,dI(b,e,g,f,c,d[2])];default:var
i=d[3],j=d[2],k=d[1],m=as(b,e,g,f,c,k,aj(0,dH(b,e,f,c,i,a(g,e,e)))),n=dI(b,e,g,f,c,i);return W(b,c,ag(b,g,c,W(b,c,dI(b,e,g,f,c,k),j,[0,b]),m),j,n)}}function
jY(c,b,a){return jV(a,jX(c,b))}function
dJ(h,g,f,e,d,c,n,a,m){var
j=n,i=m;for(;;)if(typeof
i==="number")return b(c,as(h,g,f,e,d,j,a));else{if(0===i[0]){var
k=i[1];return b(c,as(h,g,f,e,d,dJ(h,g,f,e,d,c,dJ(h,g,f,e,d,c,j,a,k),a,k),a))}var
l=i[1],j=dJ(h,g,f,e,d,c,j,a,l),i=l;continue}}function
jZ(h,a,g,f,e,d,c,b){return b?dJ(h,a,g,f,e,d,[0,a],c,b[1]):[0,a]}function
ad(a,f,c,g,e,d,b,h){switch(h[0]){case
0:return[0,h[1]];case
1:return jY(a,f,h[1]);case
2:var
i=h[2],j=h[1];if(5===j[0]){var
m=ad(a,f,c,g,e,d,b,j[1]);return $(a,c,e,d,b,ad(a,f,c,g,e,d,b,i),m)}if(5===i[0]){var
l=ad(a,f,c,g,e,d,b,i[1]);return $(a,c,e,d,b,ad(a,f,c,g,e,d,b,j),l)}var
k=ad(a,f,c,g,e,d,b,i);return ag(a,c,b,ad(a,f,c,g,e,d,b,j),k);case
3:var
n=h[1],o=ad(a,f,c,g,e,d,b,h[2]);return $(a,c,e,d,b,ad(a,f,c,g,e,d,b,n),o);case
4:var
p=h[1],q=ad(a,f,c,g,e,d,b,h[2]);return as(a,f,c,g,b,ad(a,f,c,g,e,d,b,p),q);case
5:return ac(d,ad(a,f,c,g,e,d,b,h[1]));default:var
r=h[2],s=ad(a,f,c,g,e,d,b,h[1]);return jZ(a,f,c,g,b,function(a){return a},s,r)}}function
bI(c,a){if(typeof
a!=="number")switch(a[0]){case
0:return[0,b(c,a[1])];case
2:var
d=a[1],e=bI(c,a[2]);return[2,bI(c,d),e];case
3:var
f=a[1],g=bI(c,a[2]);return[3,bI(c,f),g];case
4:return[4,bI(c,a[1])];case
5:var
h=a[2],i=a[1],j=bI(c,a[3]);return[5,bI(c,i),h,j]}return a}function
dK(d,f,e){var
b=f,c=e;for(;;){if(typeof
b!=="number")switch(b[0]){case
1:return a(d,c,b[2]);case
2:var
g=b[1],h=dK(d,b[2],c),b=g,c=h;continue;case
3:var
i=b[1],j=dK(d,b[2],c),b=i,c=j;continue;case
4:var
b=b[1];continue;case
5:var
k=b[1],l=dK(d,b[3],c),b=k,c=l;continue}return c}}function
j0(b,a){return b?[0,b[1],a]:a}function
hr(a){if(typeof
a!=="number"&&5===a[0]){var
b=a[2];return j0(b,hr(a[3]))}return 0}function
cx(b){var
a=b;for(;;){if(typeof
a!=="number")switch(a[0]){case
1:return[0,a[2],0];case
2:var
c=a[1],d=cx(a[2]);return dw(cx(c),d);case
3:var
e=a[1],f=cx(a[2]);return dw(cx(e),f);case
4:var
a=a[1];continue;case
5:var
g=a[1],h=cx(a[3]);return dw(cx(g),h)}return 0}}function
be(c,a){if(typeof
a==="number")return 0===a?0:1;else
switch(a[0]){case
0:return[0,a[1]];case
1:var
d=a[2];return[1,b(c,a[1]),d];case
2:var
e=a[1],f=be(c,a[2]);return[2,be(c,e),f];case
3:var
g=a[1],h=be(c,a[2]);return[3,be(c,g),h];case
4:return[4,be(c,a[1])];default:var
i=a[2],j=a[1],k=be(c,a[3]);return[5,be(c,j),i,k]}}var
aH=0;function
eD(e,d,c,f){if(f){var
h=f[2],g=f[1],i=a(d,c[1],g[1]);if(i){if(b(e,i[1]))return 0;var
j=eD(e,d,c,h);return j?[0,[0,g,j[1]]]:0}var
k=eD(e,d,c,h);return k?[0,[0,g,k[1]]]:0}var
l=a(d,c[1],c[1]);return l?b(e,l[1])?0:[0,[0,c,0]]:[0,[0,c,0]]}function
j1(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=eD(g,f,a[1],b);if(c){var
a=h,b=c[1];continue}return 0}return[0,b]}}function
j2(e,d,c,a){var
b=0;return ew(function(a,f){var
b=j1(e,d,c,f);return b?[0,b[1],a]:a},a,b)}function
j3(d,c,b,a){return b?j2(d,c,b,a):a}function
hs(d,c,a,b){if(a){var
e=a[2],f=j3(d,c,a[1],b);return ar(hs(d,c,e,b),f)}return aH}function
c2(a){return a?0:1}function
cy(a){if(a){var
b=a[2];return a[1]?0:b?0:1}return 0}function
cz(b,a){var
c=cy(b)?1:cy(a);return c?bo:ar(b,a)}function
eE(d,c,b,a){var
e=c2(b)?1:c2(a);return e?aH:cy(a)?b:hs(d,c,b,a)}function
aI(d,c,f,e,q,p){var
b=q,g=p;for(;;)if(typeof
g==="number")return 0===g?b?aH:bo:b?bo:aH;else
switch(g[0]){case
0:return bo;case
1:var
h=g[2],i=g[1];return b?a(f,i,h):a(e,i,h);case
2:var
j=g[2],k=g[1];if(b){var
r=aI(d,c,f,e,b,j);return cz(aI(d,c,f,e,b,k),r)}var
s=aI(d,c,f,e,b,j);return eE(d,c,aI(d,c,f,e,b,k),s);case
3:var
l=g[2],m=g[1];if(b){var
t=aI(d,c,f,e,b,l);return eE(d,c,aI(d,c,f,e,b,m),t)}var
u=aI(d,c,f,e,b,l);return cz(aI(d,c,f,e,b,m),u);case
4:var
v=g[1],b=bd(b),g=v;continue;default:var
n=g[3],o=g[1];if(b){var
w=aI(d,c,f,e,b,n);return eE(d,c,aI(d,c,f,e,bd(b),o),w)}var
x=aI(d,c,f,e,b,n);return cz(aI(d,c,f,e,bd(b),o),x)}}function
eF(e,d,c,g){if(g){var
j=g[2],f=g[1],k=a(d,c[1],f[1]);if(k){if(b(e,k[1]))return[1,[0,c[2],[0,f[2],0]]];var
h=eF(e,d,c,j);return 0===h[0]?[0,[0,f,h[1]]]:[1,h[1]]}var
i=eF(e,d,c,j);return 0===i[0]?[0,[0,f,i[1]]]:[1,i[1]]}var
l=a(d,c[1],c[1]);return l?b(e,l[1])?[1,[0,c[2],0]]:[0,[0,c,0]]:[0,[0,c,0]]}function
j4(g,f,e,d){var
a=e,b=d;for(;;){if(a){var
h=a[2],c=eF(g,f,a[1],b);if(0===c[0]){var
a=h,b=c[1];continue}return[1,c[1]]}return[0,b]}}function
j5(g,f,e,a){return ew(function(b,h){var
c=b[2],d=b[1],a=j4(g,f,e,h);return 0===a[0]?[0,[0,a[1],d],c]:[0,d,ar(c,a[1])]},a,pj)}function
j6(d,c,b,a){return b?j5(d,c,b,a):[0,a,0]}function
ht(d,c,a,b){if(a){var
g=a[1],e=ht(d,c,a[2],b),h=e[2],i=e[1],f=j6(d,c,g,b),j=f[1],k=ar(h,f[2]);return[0,ar(i,j),k]}return[0,aH,0]}function
eG(d,c,b,a){return c2(b)?[0,aH,0]:c2(a)?[0,aH,0]:cy(a)?[0,b,0]:ht(d,c,b,a)}function
j7(a,b){var
c=cy(a)?1:c2(a);return c?[0,a,[0,b,0]]:[0,a,0]}function
bp(e,d,g,f,F,E){var
b=F,c=E;for(;;)if(typeof
c==="number")return 0===c?b?[0,aH,0]:[0,bo,0]:b?[0,bo,0]:[0,aH,0];else
switch(c[0]){case
0:return[0,bo,0];case
1:var
h=c[2],k=c[1],G=b?a(g,k,h):a(f,k,h);return j7(G,h);case
2:var
H=c[2],l=bp(e,d,g,f,b,c[1]),m=l[2],n=l[1],o=bp(e,d,g,f,b,H),p=o[2],q=o[1];if(b){var
I=ar(m,p);return[0,cz(n,q),I]}var
r=eG(e,d,n,q),J=r[1];return[0,J,ar(m,ar(p,r[2]))];case
3:var
K=c[2],s=bp(e,d,g,f,b,c[1]),t=s[2],u=s[1],v=bp(e,d,g,f,b,K),w=v[2],x=v[1];if(b){var
y=eG(e,d,u,x),L=y[1];return[0,L,ar(t,ar(w,y[2]))]}var
M=ar(t,w);return[0,cz(u,x),M];case
4:var
N=c[1],b=bd(b),c=N;continue;default:var
i=c[3],O=c[1],z=bp(e,d,g,f,bd(b),O),A=z[2],j=z[1];if(b){if(cy(j)){var
c=i;continue}var
B=bp(e,d,g,f,b,i),P=B[2],C=eG(e,d,j,B[1]),Q=C[1];return[0,Q,ar(A,ar(P,C[2]))]}var
D=bp(e,d,g,f,b,i),R=D[1],S=ar(A,D[2]);return[0,cz(j,R),S]}}function
bf(c,d){if(typeof
d==="number")return 0===d?c[1]:c[2];else
switch(d[0]){case
0:return d[1];case
1:return a(c[3],d[1],d[2]);case
2:var
e=d[1],f=bf(c,d[2]),g=bf(c,e);return a(c[4],g,f);case
3:var
h=d[1],i=bf(c,d[2]),j=bf(c,h);return a(c[5],j,i);case
4:var
k=bf(c,d[1]);return b(c[7],k);default:var
l=d[1],m=bf(c,d[3]),n=bf(c,l);return a(c[6],n,m)}}function
cA(a){if(typeof
a!=="number"&&0===a[0])return[0,a[1]];return 0}function
eH(e,c,b,d){return cA(c)?[0,bf(e,a(d,c,b))]:cA(b)?[0,bf(e,a(d,c,b))]:a(d,c,b)}function
eI(e,c,b,d){return cA(c)?cA(b)?[0,bf(e,a(d,c,b))]:a(d,c,b):a(d,c,b)}function
hu(c,b,a){return c?cA(b)?a:[5,b,c,a]:[5,b,0,a]}function
bJ(c,f,e,d){if(typeof
d==="number")return 0===d?e?0:[0,c[1]]:e?[0,c[2]]:1;else
switch(d[0]){case
0:return[0,d[1]];case
1:var
g=d[2],h=d[1];return b(f,g)?[1,h,g]:[0,a(c[3],h,g)];case
2:var
r=d[2],i=bJ(c,f,e,d[1]),j=bJ(c,f,e,r);return e?eH(c,i,j,function(b,a){return[2,b,a]}):eI(c,i,j,function(b,a){return[2,b,a]});case
3:var
s=d[2],k=bJ(c,f,e,d[1]),l=bJ(c,f,e,s);return e?eI(c,k,l,function(b,a){return[3,b,a]}):eH(c,k,l,function(b,a){return[3,b,a]});case
4:var
t=d[1],m=bJ(c,f,bd(e),t),n=cA(m);return n?[0,b(c[7],n[1])]:[4,m];default:var
o=d[2],u=d[3],v=d[1],p=bJ(c,f,bd(e),v),q=bJ(c,f,e,u);return e?eI(c,p,q,function(a,b){return hu(o,a,b)}):eH(c,p,q,function(a,b){return hu(o,a,b)})}}function
j8(f,e,d){var
c=e,b=d;for(;;){if(c){var
g=c[2],h=c[1];if(b){var
i=b[2];if(a(f,h,b[1])){var
c=g,b=i;continue}return 0}return 0}return 1}}function
eJ(g,f,e,d,c,b,a){return j8(c,aI(g,f,e,d,1,b),a)}function
hv(d,c,b){return bd(a(d,c,b))}function
hw(f,e,c,b){var
d=a(e,c,b);return d?hv(f,c,b):d}function
j9(b,a){switch(b){case
0:return pk;case
1:return 1===a?pl:0===a?pm:0;case
2:return 1===a?0:[0,a];default:return 1===a?0:0===a?pn:po}}function
j_(b,a){switch(b){case
0:return[0,a];case
1:return 0===a?pp:0;case
2:return 1===a?0:pq;default:return 1===a?0:0===a?pr:[0,a]}}function
c3(c,a){return a?b(c,a[1]):0}function
c4(d,c,b){if(c){var
e=c[1];return b?a(d,e,b[1]):0}return 0}function
j$(g,f,e,d,c,b,a){var
h=a[1];return 0===a[2]?[0,[0,as(g,f,e,d,c,b,h),0]]:0}function
ka(g,f,e,d,c,b,a){var
h=b[1],i=a[1],j=j9(b[2],a[2]);return c3(function(a){return[0,[0,as(g,f,e,d,c,h,i),a]]},j)}function
dL(e,d,c,b,a){var
f=b[1],g=a[1],h=j_(b[2],a[2]);return c3(function(a){return[0,[0,ag(e,d,c,f,g),a]]},h)}function
b4(a,f,d,e,c,h,g,b){if(typeof
b==="number")return[0,[0,[0,a],0]];else
switch(b[0]){case
0:return[0,jL(b[1],g,[0,[0,a],0])];case
1:return[0,[0,dI(a,f,d,e,c,b[1]),3]];case
2:var
j=b[1],k=b4(a,f,d,e,c,h,g,b[2]);return c3(function(b){return j$(a,f,d,e,c,j,b)},k);case
3:var
l=b[1],m=b4(a,f,d,e,c,h,g,b[2]),n=b4(a,f,d,e,c,h,g,l);return c4(function(b,g){return ka(a,f,d,e,c,b,g)},n,m);case
4:var
o=b[1],p=b4(a,f,d,e,c,h,g,b[2]),q=b4(a,f,d,e,c,h,g,o);return c4(function(b,e){return dL(a,d,c,b,e)},q,p);default:var
i=b[1];return hw(c,h,a,i)?[0,[0,[0,i],2]]:0}}function
b5(b,d,f,e){var
g=e[1],h=e[2];if(0===g[0]){var
c=g[1];switch(h){case
0:return hv(d,c,b);case
1:return a(d,c,b);case
2:return a(f,c,b);default:return hw(d,f,c,b)}}return 0}function
eK(c,i,h,g,b,a,f,e){var
d=b4(c,i,h,g,b,a,f,e);return d?b5(c,b,a,d[1]):0}function
hx(e,j,d,i,c,b,a,h){var
k=h[3],l=h[2],f=ad(e,j,d,i,c,b,a,h[1]),g=ad(e,j,d,i,c,b,a,k);switch(l){case
0:return[0,$(e,d,c,b,a,f,g),0];case
1:return[0,$(e,d,c,b,a,f,g),1];case
2:return[0,$(e,d,c,b,a,g,f),3];case
3:return[0,$(e,d,c,b,a,f,g),3];case
4:return[0,$(e,d,c,b,a,g,f),2];default:return[0,$(e,d,c,b,a,f,g),2]}}function
kb(b,c){var
a=c[1];switch(c[2]){case
0:return[0,[0,a,2],[0,[0,ac(b,a),2],0]];case
1:return[0,[0,a,0],0];case
2:return[0,[0,ac(b,a),3],0];default:return[0,[0,ac(b,a),2],0]}}function
kc(d,b){var
c=b[2],a=b[1];return 1===c?[0,[0,a,2],[0,[0,ac(d,a),2],0]]:[0,[0,a,c],0]}function
hy(f,e,d,a,c){return ex(function(b,a){return b5(f,e,d,b)?a:[0,[0,[0,b,c],0],a]},aH,a)}function
hz(b,k,j,i,h,d,a,c,g,f){var
e=hx(b,k,j,i,h,d,a,g);return b5(b,a,c,e)?bo:hy(b,a,c,kb(d,e),f)}function
hA(b,k,j,i,h,d,a,c,g,f){var
e=hx(b,k,j,i,h,d,a,g);return b5(b,a,c,e)?aH:hy(b,a,c,kc(d,e),f)}function
eL(f,e){var
d=f,c=e;for(;;)switch(c[0]){case
0:return[0,c[1]];case
1:var
g=c[2],d=a(w[2],c[1],d),c=g;continue;default:var
h=c[3],i=c[2],j=c[1],k=eL(b(w[1],d),h);return[2,[4,eL(d,j),[6,[1,d],[0,i]]],k]}}function
kd(a){return eL(0,a)}function
bg(c,a){switch(a[0]){case
0:return[0,b(c,a[1])];case
1:return[1,a[1]];case
2:var
d=a[1],e=bg(c,a[2]);return[2,bg(c,d),e];case
3:var
f=a[1],g=bg(c,a[2]);return[3,bg(c,f),g];case
4:var
h=a[1],i=bg(c,a[2]);return[4,bg(c,h),i];case
5:return[5,bg(c,a[1])];default:var
j=a[2];return[6,bg(c,a[1]),j]}}function
eM(b,a){var
c=a[2],d=a[1],e=bg(b,a[3]);return[0,bg(b,d),c,e]}function
ke(q,h,f,g,c){if(typeof
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
o==="number"?p:[4,p,o]}return c}function
ay(c,b){var
d=a(p[8],b[1],[0,c[2]]);return aT(a(p[8],c[1],[0,b[2]]),d)}function
bK(c,b){var
d=a(p[8],b[1],[0,c[2]]),e=a(p[8],c[1],[0,b[2]]);return a(p[12],e,d)}function
aU(c,b){var
d=a(w[11],c[2],b[2]),e=a(p[8],b[1],[0,c[2]]),f=a(p[8],c[1],[0,b[2]]);return[0,a(p[5],f,e),d]}function
a3(c,b){var
d=a(w[11],c[2],b[2]);return[0,a(p[8],c[1],b[1]),d]}function
bL(a){var
c=a[2];return[0,b(p[6],a[1]),c]}function
b6(b,a){return aU(b,bL(a))}function
hB(b){var
a=b[1];return typeof
a==="number"?ps:0===a[0]?[0,[0,b[2]],a[1]]:[0,[1,b[2]],a[1]]}function
hC(a,b){return eA(a3,a,b)}function
hD(b,a){return typeof
a==="number"?pt:0===a[0]?hC(b,a[1]):hB(hC(b,a[1]))}function
pu(e,d,c){var
a=d,b=c;for(;;)if(typeof
a==="number")return e;else{if(0===a[0])return a[1];var
f=a[3],g=a[2],h=a[1];if(typeof
b==="number")return g;else{if(0===b[0]){var
a=f,b=b[1];continue}var
a=h,b=b[1];continue}}}function
c5(b,a,c){return typeof
a==="number"?[0,c]:0===a[0]?[1,0,b,c5(b,a[1],c)]:[1,c5(b,a[1],c),b,0]}function
eN(d,a,b,c){if(typeof
c==="number")return c5(d,a,b);else{if(0===c[0]){var
g=c[1];return typeof
a==="number"?[0,b]:0===a[0]?[1,0,g,c5(d,a[1],b)]:[1,c5(d,a[1],b),g,0]}var
e=c[3],h=c[2],f=c[1];return typeof
a==="number"?[1,f,b,e]:0===a[0]?[1,f,h,eN(d,a[1],b,e)]:[1,eN(d,a[1],b,f),h,e]}}function
bq(c){switch(c[0]){case
0:return[0,c[1]];case
1:return 0;case
2:var
d=c[1],e=bq(c[2]),f=bq(d);return c4(function(c,b){return[0,a(p[5],c,b)]},f,e);case
3:var
g=c[1],h=bq(c[2]),i=bq(g);return c4(function(c,b){return[0,a(p[7],c,b)]},i,h);case
4:var
j=c[1],k=bq(c[2]),l=bq(j);return c4(function(c,b){return[0,a(p[8],c,b)]},l,k);case
5:var
m=bq(c[1]);return c3(function(a){return[0,b(p[6],a)]},m);default:var
n=c[2],o=bq(c[1]);return c3(function(c){var
d=b(p[19],n);return[0,a(p[10],c,d)]},o)}}var
pv=p[12],pw=p[8],px=p[5],pz=0;function
kf(a,b){return eK(pz,py,px,pw,aT,pv,a,b)}var
pA=p[6],pB=p[7],pC=p[5],pD=0;function
az(a,b){return $(pD,pC,pB,pA,aT,a,b)}var
pE=p[5],pF=0;function
hE(a,b){return ag(pF,pE,aT,a,b)}var
pG=p[6],pH=p[7],pI=p[8],pJ=p[5],pL=0;function
eO(a){return ad(pL,pK,pJ,pI,pH,pG,aT,a)}var
pM=p[12],pN=0;function
cB(a){return b5(pN,aT,pM,a)}var
pO=p[5],pP=0;function
hF(a,b){return dL(pP,pO,aT,a,b)}function
c6(c){var
d=c[3],e=c[2],a=eO(c[1]),b=eO(d);switch(e){case
0:return[0,az(b,a),0];case
1:return[0,az(b,a),1];case
2:return[0,az(b,a),3];case
3:return[0,az(a,b),3];case
4:return[0,az(b,a),2];default:return[0,az(a,b),2]}}function
kg(b){var
a=b[1];switch(b[2]){case
0:var
c=[0,[0,az(pQ,a),3],0];return[0,[0,az(a,pR),3],c];case
1:return[0,[0,a,0],0];case
2:return[0,[0,az(pS,a),3],0];default:return[0,[0,az(pT,a),3],0]}}function
hG(c,a){return ex(function(b,a){return cB(b)?a:[0,[0,[0,b,c],0],a]},aH,a)}function
hH(c,b){var
a=c6(c);return cB(a)?bo:hG(b,kg(a))}function
kh(c){var
b=c[2],a=c[1];if(0!==b)switch(b-1|0){case
0:var
d=[0,[0,az(pU,a),3],0];return[0,[0,az(a,pV),3],d];case
1:return[0,[0,az(a,pW),3],0]}return[0,[0,a,b],0]}function
hI(c,b){var
a=c6(c);return cB(a)?aH:hG(b,kh(a))}function
dM(a){return bp(cB,hF,hH,hI,1,a)}function
ki(e,d){var
b=a(p[21],e,d),c=b[1];return typeof
b[2]==="number"?c:a(p[5],c,pX)}function
hJ(c,b){var
d=a(p[23],c,b);return a(p[15],d,pY)}function
dN(d){var
a=d;for(;;)switch(a[0]){case
0:return[0,0,a[1]];case
1:var
a=a[2];continue;default:var
e=a[3],b=dN(a[1]),f=b[2],g=b[1],c=dN(e),h=c[2],i=c[1];return[0,hJ(hJ(g,f),i),h]}}function
dO(b,c){switch(b[0]){case
0:return[0,a(p[22],b[1],c)];case
1:var
d=b[1];return[1,d,dO(b[2],c)];default:var
e=b[2],f=b[1],g=dO(b[3],c);return[2,dO(f,c),e,g]}}function
eP(c){var
e=dN(c),f=e[2],d=e[1];if(a(p[14],d,0)){var
g=ki(b(p[6],f),d),h=b(p[6],g);return[0,dO(c1(p[7],c,f),d),h]}return[0,c,0]}function
eQ(d){var
e=d[2],b=d[1];switch(e){case
0:var
f=dN(b),g=f[2],c=f[1];if(a(p[14],c,0))if(bd(aT(g,0)))if(bd(aT(a(p[23],c,g),c)))return 0;return[0,[0,eP(b),0]];case
1:return[0,[0,[0,b,0],e]];case
2:return[0,[0,eP(c1(p[7],b,pZ)),3]];default:return[0,[0,eP(b),3]]}}function
kj(a){var
b=a[1],c=a[2];return[0,hE(b[1],[0,b[2]]),c]}function
kk(a){return 0===a[0]?typeof
a[1]==="number"?1:0:0}var
p0=p[12],p1=p[8],p2=p[5],p4=0;function
dP(a,b){return b4(p4,p3,p2,p1,aT,p0,a,b)}function
hK(a){return 0===a?1:3<=a?1:0}function
hL(a){return[0,[1,a],3,p5]}function
kl(c,b,a){return[0,[1,c],0,[3,[1,b],[1,a]]]}function
eR(f,e){var
d=f,c=e;for(;;)switch(c[0]){case
0:return d;case
1:var
g=c[2],d=a(w[2],c[1],d),c=g;continue;default:var
h=c[3],i=c[1],j=eR(b(w[1],d),h),k=eR(d,i);return a(w[16],k,j)}}function
km(b){var
c=0;return ew(function(c,b){var
d=eR(0,b[1]);return a(w[16],c,d)},b,c)}function
hM(B,A){var
d=B,c=A;for(;;)if(typeof
c==="number")return 0;else
switch(c[0]){case
0:var
C=c[2],h=dP(d,c[1]);if(h){var
i=h[1];if(cB(i))return 1;var
d=[0,i,d],c=C;continue}return 0;case
1:var
D=c[2],j=dP(d,c[1]);if(j){var
k=eQ(j[1]);if(k){var
d=[0,kj(k[1]),d],c=D;continue}return 1}return 0;case
2:var
E=c[3],F=c[2],l=dP(d,c[1]);if(l){var
G=l[1],m=dP(d,F);if(m){var
H=m[1],n=eQ(G);if(n){var
o=n[1],q=o[1],r=q[1],I=o[2],J=q[2],s=eQ(H);if(s){var
t=s[1],u=t[1],K=t[2],L=u[2],M=u[1];if(hK(I))if(hK(K))if(kk(hE(r,M))){var
f=E,e=b(p[6],J);for(;;){if(f){var
N=f[2],O=f[1],v=hM([0,[0,az(r,[0,e]),0],d],O);if(v){var
f=N,e=a(p[5],e,p6);continue}return v}return a(p[14],e,L)}}return 0}return 1}return 1}return 0}return 0;default:var
x=c[1],P=c[2],y=km(d);if(a(w[17],x,y)){var
g=b(w[1],y),z=b(w[1],g),Q=c6(kl(x,g,z)),R=c6(hL(g)),d=[0,Q,[0,R,[0,c6(hL(z)),d]]],c=P;continue}return 0}}function
p7(b,a){return eJ(cB,hF,hH,hI,function(a){var
b=cY(b0,a);return function(a){return hM(b,a)}},b,a)}function
hN(a,b){return eK(p9,p8,aU,a3,ay,bK,a,b)}function
hO(b,a){return hz(p$,p_,aU,a3,b6,bL,ay,bK,b,a)}function
hP(b,a){return hA(qb,qa,aU,a3,b6,bL,ay,bK,b,a)}function
hQ(a){return b5(qc,ay,bK,a)}function
hR(a,b){return dL(qd,aU,ay,a,b)}function
hS(a){return ad(qf,qe,aU,a3,b6,bL,ay,a)}function
c7(a){return bp(hQ,hR,hO,hP,1,a)}function
qg(b,a){return eJ(hQ,hR,hO,hP,function(a){var
b=cY(b0,a);return function(a){return hN(b,a)}},b,a)}function
kn(a){return 0===a[0]?a[1]:b(p[18],a[1])}function
aJ(a){if(typeof
a==="number")return 0===a?qh:qi;else
switch(a[0]){case
0:return a[1];case
1:return[0,a[1],0];case
2:var
b=a[1],c=aJ(a[2]);return aU(aJ(b),c);case
3:var
d=a[1],e=aJ(a[2]);return b6(aJ(d),e);case
4:var
f=a[1],g=aJ(a[2]);return a3(aJ(f),g);case
5:var
h=a[1],i=kn(a[2]);return hD(aJ(h),i);case
6:return hB(aJ(a[1]));default:return bL(aJ(a[1]))}}function
ko(a,b){return eK(qk,qj,aU,a3,ay,bK,a,b)}function
kp(b,a){return hz(qm,ql,aU,a3,b6,bL,ay,bK,b,a)}function
kq(b,a){return hA(qo,qn,aU,a3,b6,bL,ay,bK,b,a)}function
kr(a){return b5(qp,ay,bK,a)}function
ks(a,b){return dL(qq,aU,ay,a,b)}aF(989,[0,bd,b0,oG,dw,jK,hi,jL,ar,cY,ew,ex,oH,w,jP,eA,p,aT,ph,pi,ho,aj,jV,W,jW,jX,ac,b3,c1,dE,dF,hp,hq,ag,$,dG,dH,bH,as,dI,jY,dJ,jZ,ad,bI,dK,j0,hr,cx,be,aH,bo,eD,j1,j2,j3,hs,ar,c2,cy,cz,eE,aI,eF,j4,j5,j6,ht,eG,j7,bp,bf,cA,eH,eI,hu,bJ,j8,eJ,hv,hw,j9,j_,c3,c4,j$,ka,dL,b4,b5,eK,ad,$,ag,ac,hx,kb,kc,hy,hz,hA,eL,kd,bg,eM,ke,ay,bK,aU,a3,bL,b6,hB,hC,hD,pu,c5,eN,bq,kf,az,hE,eO,cB,hF,c6,kg,hG,hH,kh,hI,dM,ki,hJ,dN,dO,eP,eQ,kj,kk,dP,hK,hL,kl,eR,km,hM,p7,hN,hO,hP,hQ,hR,hS,c7,qg,kn,aJ,ko,kp,kq,kr,ks,function(b,a){var
c=be(function(a){return eM(aJ,a)},b);return eJ(kr,ks,kp,kq,function(a){var
b=cY(b0,a);return function(a){return ko(b,a)}},c,a)}],"Micromega_plugin__Micromega");var
qr=dt,x=[0,qr,function(b,a){return b===a?1:0}],y=b(hT[1],[0,x[1]]),kt=y[13],qs=y[1],qt=y[2],qu=y[3],qv=y[4],qw=y[5],qx=y[6],qy=y[7],qz=y[8],qA=y[9],qB=y[10],qC=y[11],qD=y[12],qE=y[14],qF=y[15],qG=y[16],qH=y[17],qI=y[18],qJ=y[19],qK=y[20],qL=y[21],qM=y[22],qN=y[23],qO=y[24],qP=y[25],qQ=y[26],qR=y[27],qS=y[28],qT=y[29],qU=y[30],qV=y[31],qW=y[32],qX=y[33],qY=y[34],qZ=y[35],q0=y[36],q1=y[37],q2=y[38],q3=y[39],t=[0,qs,qt,qu,qv,qw,qx,qy,qz,qA,qB,qC,qD,kt,qE,qF,qG,qH,qI,qJ,qK,qL,qM,qN,qO,qP,qQ,qR,qS,qT,qU,qV,qW,qX,qY,qZ,q0,q1,q2,q3,function(c,b){return a(kt,function(a){return h(n[1],c,q4,a)},b)}],z=b(cC[1],[0,x[1]]),ku=z[26],q5=z[1],q6=z[2],q7=z[3],q8=z[4],q9=z[5],q_=z[6],q$=z[7],ra=z[8],rb=z[9],rc=z[10],rd=z[11],re=z[12],rf=z[13],rg=z[14],rh=z[15],ri=z[16],rj=z[17],rk=z[18],rl=z[19],rm=z[20],rn=z[21],ro=z[22],rp=z[23],rq=z[24],rr=z[25],rs=z[27],rt=z[28],ru=z[29],rv=z[30],rw=z[31],rx=z[32],ry=z[33],rz=z[34],rA=z[35],rB=z[36],rC=z[37],rD=z[38],s=[0,q5,q6,q7,q8,q9,q_,q$,ra,rb,rc,rd,re,rf,rg,rh,ri,rj,rk,rl,rm,rn,ro,rp,rq,rr,ku,rs,rt,ru,rv,rw,rx,ry,rz,rA,rB,rC,rD,function(c,b){return a(ku,c-1|0,b)[3]}];function
kv(i,d,c,h){var
b=h;for(;;){if(b){var
f=b[2],g=b[1];if(f){a(d,c,g);a(e[55],c,i);var
b=f;continue}return a(d,c,g)}return 0}}function
rE(a,c){try{var
d=b(a,0);b(c,0);return d}catch(a){a=q(a);try{b(c,0)}catch(b){throw a}throw a}}function
rF(e,d){var
a=e;for(;;){if(a){var
f=a[2],c=b(a[1][1],d);if(c)return c;var
a=f;continue}return 0}}function
kw(e,d){var
c=0,b=d;for(;;){if(b){var
i=b[2],j=b[1],f=function(d){return function(c,b){return[0,a(e,d,b),c]}}(j),c=h(g[20],f,c,b),b=i;continue}return c}}function
kx(g,f,e){var
c=f,b=e;for(;;){if(c){var
h=c[2],i=c[1];if(b){var
d=b[2];if(a(g,i,b[1])){var
c=h,b=d;continue}var
b=d;continue}return 0}return 1}}function
bM(i,a){function
c(e,a){var
c=e[2],d=e[1];if(d)return[0,d,[0,a,c]];var
f=b(i,a);return f?[0,[0,[0,f[1],a]],c]:[0,d,[0,a,c]]}return h(g[20],c,rG,a)}function
rH(j,p,i){var
m=bM(j,i),n=m[1];if(n){var
o=n[1],g=o[1],f=o[2],c=0,d=m[2];for(;;){if(d){var
h=d[2],e=d[1],k=b(j,e);if(k){var
l=k[1];if(a(p,l,g)){var
g=l,q=[0,f,c],f=e,c=q,d=h;continue}var
c=[0,e,c],d=h;continue}var
c=[0,e,c],d=h;continue}return[0,[0,[0,g,f]],c]}}return[0,0,i]}function
rI(e,d){var
a=d;for(;;){if(a){var
f=a[2],c=b(e,a[1]);if(c)return c[1];var
a=f;continue}throw K}}function
ky(b,a){try{var
c=[0,rI(b,a)];return c}catch(a){a=q(a);if(a===K)return 0;throw a}}function
kz(i,a){function
c(c,a){var
d=c[2],e=c[1],f=b(i,a);return f?[0,[0,[0,f[1],a],e],d]:[0,e,[0,a,d]]}return h(g[20],c,rJ,a)}function
eS(f,c){function
d(c,a){var
d=c[1],g=c[2],e=b(f,a);return e?[0,[0,e[1],d],1]:[0,[0,a,d],g]}var
a=h(g[20],d,rK,c),e=a[1];return a[2]?[0,e]:0}function
kA(e,c){var
d=0;function
a(a,d){var
c=b(e,d);return c?[0,c[1],a]:a}return h(g[20],a,d,c)}function
kB(j,i,c){function
d(l,k){var
c=l,d=k;for(;;){var
f=bM(j,d),g=f[1];if(g){var
h=f[2],m=g[1],n=a(e[26],h,c),o=kA(b(i,m),n),c=a(e[26],o,c),d=h;continue}return c}}try{var
f=d(0,c);return f}catch(a){a=q(a);b(eT[4],e[28]);throw a}}function
kC(d,c){var
b=a(o[17],d,c),e=a(o[15],d,b),f=a(o[15],c,b),g=a(o[10],e,f);return a(o[10],b,g)}function
bh(a){return 2===a[0]?b(dQ[3],a[1]):o[2]}function
ak(a){switch(a[0]){case
0:return b(o[36],a[1]);case
1:return a[1];default:return b(dQ[2],a[1])}}function
eU(e,d){var
a=d;for(;;){var
c=b(e,a);if(c){var
a=c[1];continue}return a}}function
kD(e,d){var
a=e;for(;;){if(a){var
f=a[2],c=b(a[1],d);if(c)return[0,c[1]];var
a=f;continue}return 0}}function
kE(a){return a?kE(a[1])+1|0:0}function
eV(a){return typeof
a==="number"?1:0===a[0]?1+(2*eV(a[1])|0)|0:2*eV(a[1])|0}function
rL(a){return a?eV(a[1]):0}function
cD(a){return typeof
a==="number"?1:0===a[0]?1+(2*cD(a[1])|0)|0:2*cD(a[1])|0}function
eW(b){if(typeof
b==="number")return o[2];else{if(0===b[0]){var
c=eW(b[1]),d=a(o[11],2,c);return a(o[7],1,d)}var
e=eW(b[1]);return a(o[11],2,e)}}function
hU(a){if(typeof
a==="number")return o[1];else{if(0===a[0])return eW(a[1]);var
c=eW(a[1]);return b(o[3],c)}}function
kF(a){return typeof
a==="number"?0:0===a[0]?cD(a[1]):-cD(a[1])|0}function
kG(b){var
c=b[1],e=[1,hU([0,b[2]])],f=[1,hU(c)];return a(d[9],f,e)}function
kH(a){return 0===a?0:[0,kH(a-1|0)]}function
c8(b){return a(x[2],b,1)?0:a(x[2],b&1,1)?[0,c8(b>>>1|0)]:[1,c8(b>>>1|0)]}function
rM(b){if(0<=b)return a(x[2],b,0)?0:[0,c8(b)];throw[0,aV,rN]}function
hV(b){return a(x[2],b,1)?0:a(x[2],b&1,1)?[0,hV(b>>>1|0)]:[1,hV(b>>>1|0)]}function
rO(a){var
b=dt(a,0);return 0===b?0:1===b?[0,c8(a)]:[1,c8(-a|0)]}function
eX(d){var
f=b(o[36],2);function
c(b){if(a(o[24],b,o[2]))return 0;var
d=a(o[14],b,f),e=d[1];return a(o[24],o[2],d[2])?[0,c(e)]:[1,c(e)]}return c(d)}function
kI(a){var
c=b(o[22],a);return 0===c?0:1===c?[0,eX(a)]:[1,eX(b(o[3],a))]}function
rP(a){var
b=eX(bh(a));return[0,kI(ak(a)),b]}function
rQ(e){var
c=e;for(;;){if(c){var
f=c[2],d=b(c[1],0);if(a(x[2],d,0)){var
c=f;continue}return d}return 0}}function
rR(g,f,e){var
c=f,b=e;for(;;){if(c){if(b){var
h=b[2],i=c[2],d=a(g,c[1],b[1]);if(a(x[2],d,0)){var
c=i,b=h;continue}return d}return 1}return b?-1:0}}function
rS(a){return a}function
rT(a){return a+1|0}var
rU=e[6];function
rV(d,c){var
f=b(e[22],c);return a(e[55],d,f)}var
rW=x[1];function
rX(a){return a}var
R=b(hT[1],[0,rW]),rY=R[1],rZ=R[2],r0=R[3],r1=R[4],r2=R[5],r3=R[6],r4=R[7],r5=R[8],r6=R[9],r7=R[10],r8=R[11],r9=R[12],r_=R[13],r$=R[15],sa=R[16],sb=R[17],sc=R[18],sd=R[19],se=R[20],sf=R[21],sg=R[22],sh=R[24],si=R[26],sj=R[28];function
sk(c){for(;;)try{var
d=a(O[15],0,c)[2];return d}catch(a){a=q(a);if(a[1]===O[1]){var
b=a[2];if(typeof
b==="number")if(11===b)continue}throw a}}function
kJ(c,t,s){var
f=a(O[67],0,0),i=f[2],j=f[1],k=a(O[67],0,0),l=k[2],m=k[1],o=a(O[67],0,0),p=o[2],u=o[1],v=U(O[69],c,t,j,l,p),r=b(O[31],i);a(e[61],r,s);b(e[52],r);var
d=sk(v);function
w(e){var
c=[0,j,[0,i,[0,m,[0,l,[0,u,[0,p,0]]]]]];function
d(a){try{var
c=b(O[24],a);return c}catch(a){return 0}}return a(g[15],d,c)}return rE(function(p){switch(d[0]){case
0:var
a=d[1];if(0===a){var
f=b(O[30],m);try{var
j=b(eY[3],f);return j}catch(a){a=q(a);var
g=b(eT[1],a),i=h(n[4],sl,c,g);return b(e[3],i)}}var
k=h(n[4],sm,c,a);return b(e[3],k);case
1:var
l=h(n[4],sn,c,d[1]);return b(e[3],l);default:var
o=h(n[4],so,c,d[1]);return b(e[3],o)}},w)}function
hW(a){switch(a){case
0:return 0;case
1:return 1;case
2:return 2;default:return 3}}function
sp(b,a){var
c=hW(a);return hW(b)===c?1:0}function
sq(c,b){var
d=hW(b);return a(cE[2][1],c,d)}function
dR(d,c){var
b=d,a=c;for(;;){if(typeof
b==="number"){if(typeof
a==="number")return 1}else
if(0===b[0]){var
e=b[1];if(typeof
a!=="number"&&1!==a[0]){var
b=e,a=a[1];continue}}else{var
f=b[1];if(typeof
a!=="number"&&0!==a[0]){var
b=f,a=a[1];continue}}return 0}}function
kK(c,a){if(typeof
c==="number"){if(typeof
a==="number")return 1;var
b=0}else
if(0===c[0]){var
h=c[1];if(typeof
a==="number")var
d=1;else
if(1===a[0])var
d=1;else
var
g=a[1],f=h,b=1,d=0;if(d)var
b=0}else{var
i=c[1];if(typeof
a==="number")var
e=1;else
if(0===a[0])var
e=1;else
var
g=a[1],f=i,b=1,e=0;if(e)var
b=0}return b?dR(f,g):0}function
sr(b,a){var
d=a[2],e=b[2],c=kK(b[1],a[1]);return c?dR(e,d):c}function
kL(d,j,i){var
c=j,b=i;for(;;){switch(c[0]){case
0:var
k=c[1];if(0===b[0])return a(d,k,b[1]);break;case
1:var
l=c[2],m=c[1];if(1===b[0]){var
n=b[2],e=dR(m,b[1]);if(e){var
c=l,b=n;continue}return e}break;default:var
o=c[3],p=c[2],q=c[1];if(2===b[0]){var
r=b[3],s=b[2],f=kL(d,q,b[1]);if(f){var
g=dR(p,s);if(g){var
c=o,b=r;continue}var
h=g}else
var
h=f;return h}}return 0}}function
ss(f,e,c,b){var
g=b[2],h=c[2],d=a(f,c[1],b[1]);return d?a(e,h,g):d}function
st(e){function
d(g,f){var
c=g,b=f;for(;;)switch(b[0]){case
0:var
h=b[1];return a(e,a(cE[2][1],c,1),h);case
1:var
i=b[2],j=cD(b[1]),k=a(cE[2][1],c,1),c=a(cE[2][1],k,j),b=i;continue;default:var
l=b[3],m=b[1],n=cD(b[2]),o=a(cE[2][1],c,2),c=d(a(cE[2][1],o,n),m),b=l;continue}}return d}function
su(e,d,c,b){var
f=b[2];return a(d,a(e,c,b[1]),f)}function
eZ(e,d,c){var
f=b(e,c);return a(cE[2][1],d,f)}function
sv(b,a){return eZ(bi[27],b,a)}function
sw(a,b){return eZ(kF,a,b)}function
sx(a){var
c=kG(a);return b(bi[27],c)}var
aa=[0,sp,dR,kK,sr,kL,ss,sq,st,su,sw,function(a,b){return eZ(sx,a,b)},sv,eZ],aA=[0,hU,kF,kG,eV,rL,kE,cD],A=[0,c8,kI,rM,kH,rP,hV,rO,eX],cF=[0,rY,rZ,r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r_,r$,sa,sb,sc,sd,se,sf,sg,sh,si,sj],c9=[0,rV,rT,rU,rS,rX],hX=[0,rR,rQ];aF(1005,[0,x,t,s,ak,bh,hX,c9,cF,kv,A,aA,aa,kC,kw,rF,kx,bM,kz,rH,ky,eU,eS,kB,kA,kD,kJ],"Micromega_plugin__Mutils");function
dS(k,j){var
c=k,b=j;for(;;){if(c){var
e=c[1],l=c[2],m=e[2],n=e[1];if(b){var
f=b[1],o=b[2],p=f[2],g=a(x[2],n,f[1]);if(g){var
h=a(d[26],m,p);if(h){var
c=l,b=o;continue}var
i=h}else
var
i=g;return i}return 0}return b?0:1}}function
kM(f){var
c=0,a=f;for(;;){if(a){var
e=a[1],g=a[2],h=e[1],i=[0,h,b(d[56],e[2])],c=c+b(bi[27],i)|0,a=g;continue}return b(bi[27],c)}}var
P=0;function
b7(a){if(a){var
b=a[1];if(0===b[1])var
c=b[2],d=0===c[0]?0===c[1]?a[2]?0:1:0:0;else
var
d=0;if(!d)return 0}return 1}function
kN(g,e,i){var
c=i[2],f=i[1];if(a(x[2],f,0)){if(a(d[32],sy,c))return 0;var
k=b(d[40],c);return h(n[1],e,sz,k)}if(0===c[0]){var
j=c[1]+1|0;if(!(2<j>>>0))switch(j){case
0:return F(n[1],e,sB,g,f);case
1:return 0;default:return a(g,e,f)}}var
l=b(d[40],c);return U(n[1],e,sA,l,g,f)}function
e0(d,c,b){if(b){var
f=b[2],g=b[1];if(f){var
h=function(a,b){return e0(d,a,b)},i=function(a,b){return kN(d,a,b)};return V(n[1],c,sG,i,g,h,f)}return kN(d,c,g)}return a(e[55],c,sH)}function
e1(b,a){return h(n[1],b,sI,a)}function
cG(b,a){return e0(e1,b,a)}function
sJ(e,c){function
f(e,c){function
f(c){function
f(f,i){var
c=i[2],e=i[1];if(a(x[2],e,0)){if(a(d[32],sC,c))return 0;var
j=b(d[40],c);return h(n[1],f,sD,j)}if(0===c[0]){var
g=c[1]+1|0;if(!(2<g>>>0))switch(g){case
0:return F(n[1],f,sF,e1,e);case
1:return 0;default:return e1(f,e)}}var
k=b(d[40],c);return U(n[1],f,sE,k,e1,e)}return F(n[1],e,sK,f,c)}return a(g[15],f,c)}return F(n[1],e,sL,f,c)}function
cH(b){function
e(i,h){var
c=i,b=h;for(;;){if(b){var
f=b[2],g=b[1];if(a(d[31],g,sM))return[0,[0,c,g],e(c+1|0,f)];var
c=c+1|0,b=f;continue}return 0}}return e(0,b)}function
hZ(a){function
b(c,a){if(a){var
d=a[1],e=a[2],f=d[2];return c===d[1]?[0,f,b(c+1|0,e)]:[0,hY,b(c+1|0,a)]}return 0}return b(0,a)}function
c_(e,c,b){return a(d[26],c,sN)?b:[0,[0,e,c],b]}function
h0(f,d,c){if(c){var
h=c[2],i=c[1],j=i[2],g=i[1],k=a(x[1],f,g)+1|0;if(2<k>>>0)return b(e[3],sO);switch(k){case
0:return c_(f,b(d,hY),c);case
1:return c_(g,b(d,j),h);default:return[0,[0,g,j],h0(f,d,h)]}}return c_(f,b(d,hY),0)}function
H(f,d,c){if(c){var
h=c[2],i=c[1],g=i[1],k=i[2],j=a(x[1],f,g)+1|0;if(2<j>>>0)return b(e[3],sP);switch(j){case
0:return c_(f,d,c);case
1:return c_(g,d,h);default:return[0,[0,g,k],H(f,d,h)]}}return c_(f,d,0)}function
e2(b){return a(d[26],b,sQ)?0:[0,[0,0,b],0]}function
at(b,c){if(0===b[0]){var
e=b[1];if(0===e)return 0;if(1===e)return c}function
f(c){var
e=c[1];return[0,e,a(d[7],b,c[2])]}return a(g[17],f,c)}function
c$(c,b){if(a(d[31],c,sR)){var
e=function(b){var
e=b[1];return[0,e,a(d[9],b[2],c)]};return a(g[17],e,b)}return b}function
b8(c){function
e(a){var
c=a[1];return[0,c,b(d[3],a[2])]}return a(g[17],e,c)}function
aK(q,p){var
c=q,b=p;for(;;){if(c){if(b){var
e=b[2],h=b[1],i=h[2],j=h[1],f=c[2],k=c[1],l=k[2],g=k[1],m=a(x[1],g,j);if(0===m){var
n=a(d[2],l,i);if(a(d[32],sS,n)){var
c=f,b=e;continue}return[0,[0,g,n],aK(f,e)]}return 0<=m?[0,[0,j,i],aK(e,c)]:[0,[0,g,l],aK(f,b)]}var
o=c}else
var
o=b;return o}}function
e3(c,r,b,q){var
f=r,e=q;for(;;){if(f){if(e){var
g=e[2],j=e[1],k=j[2],l=j[1],h=f[2],m=f[1],n=m[2],i=m[1],o=a(x[1],i,l);if(0===o){var
s=a(d[6],b,k),t=a(d[6],c,n),p=a(d[1],t,s);if(a(d[32],sT,p)){var
f=h,e=g;continue}return[0,[0,i,p],e3(c,h,b,g)]}if(0<=o){var
u=e3(c,f,b,g);return[0,[0,l,a(d[6],b,k)],u]}var
v=e3(c,h,b,e);return[0,[0,i,a(d[6],c,n)],v]}return at(c,f)}return at(b,e)}}function
e4(f,e,c,b){if(a(d[26],f,sU))if(a(d[26],c,sV))return aK(e,b);return e3(f,e,c,b)}function
sW(e,c){var
f=0,g=[0,function(b){return a(d[37],e[2],c[2])},f],h=[0,function(b){return a(x[1],e[1],c[1])},g];return b(hX[2],h)}var
h1=b(hX[1],sW);function
au(i,h){var
b=h;for(;;){if(b){var
d=b[1],f=b[2],g=d[2],e=a(x[1],d[1],i);if(-1===e){var
b=f;continue}var
c=0===e?[0,[0,g,b]]:0}else
var
c=0;return c?c[1][1]:sX}}function
kO(a){if(a){var
b=0===a[1][1]?a[2]?0:1:0;if(!b)return 0}return 1}function
da(a){if(a){var
b=a[1];if(0===b[1])return b[2]}return sY}function
a4(a){if(a){var
b=a[1];return[0,[0,b[1],b[2],a[2]]]}return 0}function
kP(c){var
a=c;for(;;){if(a){var
b=a[2],d=a[1][1];if(b){var
a=b;continue}return d+1|0}return 1}}function
h2(b){var
c=t[1];function
d(c,b){return a(t[4],b[1],c)}return h(g[20],d,c,b)}function
br(a){if(a){var
b=a[1];if(0===b[1])return[0,b[2],a[2]]}return[0,sZ,a]}function
s0(b,f){var
a=f;for(;;){if(a){var
c=a[2],d=a[1],e=d[1],g=d[2];if(ab(b,e))return[0,g,c];if(cs(b,e))return[0,s1,a];var
a=c;continue}return[0,s2,P]}}function
h3(a){return a?[0,a[1],a[2]]:s3}function
h4(d,c,b){if(b){var
e=b[2],f=b[1],g=f[2],h=f[1],i=a(x[1],d,h)+1|0;if(2<i>>>0)throw[0,aV,s4];switch(i){case
0:return b;case
1:return e4(g,c,s5,e);default:return aK([0,[0,h,g],0],h4(d,c,e))}}return 0}function
X(c,b,a){function
d(b,a){return h(c,b,a[1],a[2])}return h(g[20],d,b,a)}function
s6(g,f,e){var
b=f,a=e;for(;;){if(a){var
c=a[1],i=a[2],d=h(g,b,c[1],c[2]);if(d){var
b=d[1],a=i;continue}return 0}return[0,b]}}function
h5(f,e){var
b=e;for(;;){if(b){var
c=b[1],g=b[2],d=a(f,c[1],c[2]);if(d)return[0,d[1]];var
b=g;continue}return 0}}function
e5(c,b){function
d(b){return a(c,b[1],b[2])}return a(g[27],d,b)}function
kQ(c,b){function
d(a){return[0,a[1]-c|0,a[2]]}return a(g[17],d,b)}function
s7(c,b){function
d(a){return[0,a[1]+c|0,a[2]]}return a(g[17],d,b)}function
h6(c){var
d=o[1],b=X(function(c,h,b){var
d=o[2],e=bh(b),f=a(o[23],e,d);if(a(x[2],f,0)){var
g=ak(b);return a(o[17],c,g)}throw[0,aV,s8]},d,c),e=a(o[23],b,o[1]);return a(x[2],e,0)?o[2]:b}function
db(b){var
e=o[2],f=X(function(b,c,a){return kC(b,bh(a))},e,b),h=o[1],c=X(function(c,e,b){var
d=ak(b);return a(o[17],c,d)},h,b),i=a(o[23],c,o[1]),j=a(x[2],i,0)?o[2]:c;function
k(b){var
c=b[1],e=a(d[6],b[2],[1,f]);return[0,c,a(d[9],e,[1,j])]}return a(g[17],k,b)}function
kR(n,m,l){var
c=m,b=l;for(;;){if(c)if(b){var
e=b[2],f=b[1],g=f[2],h=f[1],i=c[2],j=c[1],k=j[2],d=j[1];if(a(x[2],d,h)){if(a(n,k,g))return[0,[0,d,k,g]];var
c=i,b=e;continue}if(d<h){var
c=i;continue}var
b=e;continue}return 0}}function
e6(m,l){var
e=s9,c=m,b=l;for(;;){if(c)if(b){var
f=b[2],g=b[1],h=g[1],i=c[2],j=c[1],k=j[1],n=g[2],o=j[2];if(k===h){var
p=a(d[6],o,n),e=a(d[1],e,p),c=i,b=f;continue}if(cs(k,h)){var
c=i;continue}var
b=f;continue}return e}}function
s_(c,b){function
d(b){return a(c,b[1],b[2])}return a(g[17],d,b)}function
s$(c){if(c){var
e=c[1],f=c[2],i=[0,e[1],e[2]],j=function(e,c){var
f=c[2],g=e[2],h=c[1],i=e[1],j=b(d[15],f),k=b(d[15],g);return a(d[27],k,j)?[0,i,g]:[0,h,f]};return[0,h(g[20],j,i,f)]}return 0}function
ta(c){function
d(b){return a(c,b[1],b[2])}return b(g[37],d)}function
tb(a){return H(a,tc,P)}aF(1006,[0,kM,dS,h1,e0,cG,sJ,h2,da,br,s0,h3,e2,kO,P,b7,au,H,tb,h0,kP,a4,cH,hZ,kQ,s7,h6,db,aK,at,e4,h4,c$,b8,X,s6,h5,e5,kR,e6,s_,s$,ta,[0,function(a){if(a){var
b=a[1],c=b[1];if(!a[2]){var
f=b[2];return 0===c?0:[0,[0,td,c,f]]}if(0===c){var
d=a[2];if(!d[2]){var
e=d[1];return[0,[0,b[2],e[1],e[2]]]}}}return 0}]],"Micromega_plugin__Vect");function
bj(f,c){if(typeof
c==="number")return a(e[55],f,te);else
switch(c[0]){case
0:var
l=b(d[40],c[1]);return a(e[55],f,l);case
1:return h(n[1],f,tf,c[1]);case
2:return F(n[1],f,tg,bj,c[1]);case
3:var
g=c[1];return V(n[1],f,th,bj,g[1],bj,g[2]);case
4:var
i=c[1];return V(n[1],f,ti,bj,i[1],bj,i[2]);case
5:var
j=c[1];return V(n[1],f,tj,bj,j[1],bj,j[2]);default:var
k=c[1];return U(n[1],f,tk,bj,k[1],k[2])}}function
dc(e,c){switch(c[0]){case
0:return h(n[1],e,tl,c[1]);case
1:return h(n[1],e,tm,c[1]);case
2:return h(n[1],e,tn,c[1]);case
3:var
f=b(d[40],c[1]);return h(n[1],e,to,f);case
4:var
g=b(d[40],c[1]);return h(n[1],e,tp,g);case
5:var
i=b(d[40],c[1]);return h(n[1],e,tq,i);case
6:return F(n[1],e,tr,bj,c[1]);case
7:return a(n[1],e,ts);case
8:return V(n[1],e,tt,bj,c[1],dc,c[2]);case
9:return V(n[1],e,tu,dc,c[1],dc,c[2]);default:return V(n[1],e,tv,dc,c[1],dc,c[2])}}aF(1007,[0,bj,dc],"Micromega_plugin__Sos_types");function
dT(b,a){return 0===hb(b,a)?1:0}function
dU(b,a){return hb(b,a)<0?1:0}function
tw(b,a){return hb(b,a)<=0?1:0}function
al(d,c,a){return b(d,b(c,a))}function
dV(b){return a(d[14],tx,[0,b])}function
e7(b){return a(d[14],ty,[0,b])}function
kS(c){var
e=b(d[54],c),a=b(dQ[5],e),f=b(dQ[3],a),g=b(d[51],f),h=b(dQ[2],a);return[0,b(d[51],h),g]}function
tz(a){return a[1]}function
h8(a){return al(tz,kS,a)}function
tA(a){return a[2]}function
e8(a){return al(tA,kS,a)}function
e9(e,c){var
f=b(d[52],c),g=b(d[52],e),h=a(o[17],g,f);return b(d[51],h)}function
e_(e,c){if(a(d[26],e,h7))if(a(d[26],c,h7))return h7;var
f=e9(e,c),g=a(d[6],e,c),h=a(d[9],g,f);return b(d[15],h)}function
bt(d,c){if(c){var
f=c[2],g=c[1];return f?a(d,g,bt(d,f)):g}return b(e[3],tB)}function
e$(d,b,c){if(b){var
e=b[1],f=e$(d,b[2],c),i=function(c,b){return[0,a(d,e,c),b]};return h(g[21],i,c,f)}return 0}function
fa(a){return h(g[21],e[17],a,tC)}function
bk(c){var
a=cX(c)-1|0,b=0;for(;;){if(0<=a){var
d=[0,h(aW[4],c,a,1),b],a=a-1|0,b=d;continue}return b}}function
h9(f,e,d){var
c=f,a=d;for(;;){if(1<=c){var
c=c-1|0,a=b(e,a);continue}return a}}function
Q(a,b){return b<a?0:[0,a,Q(a+1|0,b)]}function
h_(d,c){var
a=c;for(;;){if(a){var
f=a[2],g=a[1];try{var
h=b(d,g);return h}catch(b){b=q(b);if(b[1]===fb){var
a=f;continue}throw b}}return b(e[3],tD)}}function
kT(d,c){var
a=c;for(;;){if(a){var
e=a[2],b=dT(d,a[1]);if(b)return b;var
a=e;continue}return 0}}function
tE(b,a){return kT(b,a)?a:[0,b,a]}function
h$(b,a){return h(g[21],tE,b,a)}function
ia(c,b){function
d(a){return 1-kT(a,b)}return a(g[35],d,c)}function
dW(a,d,c){var
e=b(a,c);return dU(b(a,d),e)}function
dd(d,c){var
a=c;for(;;){if(a){var
e=a[2];b(d,a[1]);var
a=e;continue}return 0}}function
aX(d,c){if(c){var
f=c[1],i=c[2],j=b(d,f),h=a(g[37],j,i),k=h[2],l=[0,f,aX(d,h[1])],m=aX(d,k);return a(e[26],m,l)}return 0}function
kU(a){if(a){var
b=a[2];if(b){var
d=a[1],e=b[1],c=kU(b);return dT(d,e)?c:c===b?a:[0,d,c]}}return a}function
de(a){return kU(aX(tw,a))}var
u=0;function
av(a){return typeof
a==="number"?1:0}function
kV(c,a){if(a){var
d=a[1],e=d[2],f=d[1],g=kV(c,a[2]);return[0,[0,f,b(c,e)],g]}return 0}function
aL(b,a){if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=a[1];return[0,c,kV(b,a[2])]}var
d=a[3],e=a[2],f=a[1],g=aL(b,a[4]);return[1,f,e,aL(b,d),g]}}function
D(f,j,i){var
c=j,a=i;for(;;)if(typeof
a==="number")return c;else{if(0===a[0]){var
d=c,b=a[2];for(;;){if(b){var
e=b[1],g=b[2],d=h(f,d,e[1],e[2]),b=g;continue}return d}}var
k=a[4],c=D(f,c,a[3]),a=k;continue}}function
kW(c,a,b){if(a){var
d=a[1],e=d[2],f=d[1];return h(c,f,e,kW(c,a[2],b))}return b}function
dX(c,e,d){var
a=e,b=d;for(;;)if(typeof
a==="number")return b;else{if(0===a[0])return kW(c,a[2],b);var
f=a[3],g=dX(c,a[4],b),a=f,b=g;continue}}function
cI(b,e,g,d){var
c=b^g,a=c&(-c|0),f=b&(a-1|0);return 0===(b&a)?[1,f,a,e,d]:[1,f,a,d,e]}function
kX(a,b){var
c=a[1];if(b){var
d=b[2],e=b[1],f=e[1];return dT(c,f)?[0,a,d]:dU(c,f)?[0,a,b]:[0,e,kX(a,d)]}return[0,a,0]}function
fc(f,e,d,c){if(d){if(c){var
j=c[2],g=c[1],k=g[1],l=d[2],h=d[1],i=h[1],o=g[2],p=h[2];if(dU(i,k))return[0,h,fc(f,e,l,c)];if(dU(k,i))return[0,g,fc(f,e,d,j)];var
m=a(f,p,o),n=fc(f,e,l,j);return b(e,m)?n:[0,[0,i,m],n]}return d}return c}function
G(d,e){var
c=b(bi[27],d);function
g(a){if(typeof
a==="number")return[0,c,[0,[0,d,e],0]];else{if(0===a[0]){var
h=a[1],k=a[2];return h===c?[0,h,kX([0,d,e],k)]:cI(h,a,c,[0,c,[0,[0,d,e],0]])}var
i=a[4],j=a[3],b=a[2],f=a[1];return(c&(b-1|0))!==f?cI(f,a,c,[0,c,[0,[0,d,e],0]]):0===(c&b)?[1,f,b,g(j),i]:[1,f,b,j,g(i)]}}return g}function
aM(e,d,b,a){if(typeof
b==="number")return a;else
if(0===b[0]){var
m=b[1],F=b[2];if(typeof
a==="number")var
t=0;else{if(0===a[0]){var
w=a[1],G=a[2];if(m===w){var
x=fc(e,d,F,G);return 0===x?0:[0,m,x]}return cI(m,b,w,a)}var
y=a,q=a[4],p=a[3],j=a[2],i=a[1],o=b,n=m,t=1}}else{var
k=b[4],l=b[3],g=b[2],c=b[1];if(typeof
a==="number")var
t=0;else{if(0!==a[0]){var
r=a[4],s=a[3],h=a[2],f=a[1];if(g<h){if((f&(g-1|0))!==c)return cI(c,b,f,a);if(0===(f&g)){var
B=aM(e,d,l,a);return av(B)?k:[1,c,g,B,k]}var
C=aM(e,d,k,a);return av(C)?l:[1,c,g,l,C]}if(h<g){if((c&(h-1|0))!==f)return cI(c,b,f,a);if(0===(c&h)){var
D=aM(e,d,b,s);return av(D)?r:[1,f,h,D,r]}var
E=aM(e,d,b,r);return av(E)?s:[1,f,h,s,E]}if(c===f){var
u=aM(e,d,l,s),v=aM(e,d,k,r);return av(u)?v:av(v)?u:[1,c,g,u,v]}return cI(c,b,f,a)}var
y=b,q=k,p=l,j=g,i=c,o=a,n=a[1],t=1}}if(t){if((n&(j-1|0))===i){if(0===(n&j)){var
z=aM(e,d,o,p);return av(z)?q:[1,i,j,z,q]}var
A=aM(e,d,o,q);return av(A)?p:[1,i,j,p,A]}return cI(n,o,i,y)}return b}function
a5(c,a){return b(G(c,a),u)}function
ib(c){var
a=c;for(;;)if(typeof
a==="number")return b(e[3],tF);else{if(0===a[0])return b(g[5],a[2]);var
a=a[3];continue}}function
kY(k,e,c){var
h=b(bi[27],c),a=k;for(;;){if(typeof
a!=="number"){if(0!==a[0]){var
m=a[4],n=a[3],o=0===(h&a[2])?n:m,a=o;continue}var
l=a[2];if(a[1]===h){var
d=l;for(;;){if(d){var
f=d[1],g=f[1],i=d[2],j=f[2];if(dT(c,g))return j;if(0<hb(c,g)){var
d=i;continue}return b(e,c)}return b(e,c)}}}return b(e,c)}}function
fd(a){function
c(a){return b(e[3],tG)}return function(b){return kY(a,c,b)}}function
bl(c,b,a){return kY(c,function(b){return a},b)}function
kZ(b,a){if(a){var
c=a[2],d=a[1],e=d[1];if(dT(b,e))return c;if(dU(b,e))return a;var
f=kZ(b,c);return f===c?a:[0,d,f]}return 0}function
ic(k){var
e=b(bi[27],k);function
f(a){if(typeof
a!=="number")if(0===a[0]){var
l=a[2],m=a[1];if(m===e){var
g=kZ(k,l);return g===l?a:0===g?0:[0,m,g]}}else{var
b=a[4],c=a[3],d=a[2],h=a[1];if((e&(d-1|0))===h){if(0===(e&d)){var
i=f(c);return i===c?a:av(i)?b:[1,h,d,i,b]}var
j=f(b);return j===b?a:av(j)?c:[1,h,d,c,j]}}return a}return f}function
cJ(a){var
b=0;return de(D(function(c,b,a){return[0,[0,b,a],c]},b,a))}function
fe(a){var
b=0;return de(D(function(b,a,c){return[0,a,b]},b,a))}var
df=[bc,tH,bb(0)];function
tI(a){return bZ(a,0)}var
tJ=a(e[17],k4,k5),tK=a(e[17],k3,tJ),tL=a(e[17],k2,tK),tM=a(e[17],k1,tL),tO=bk(a(e[17],k0,tM)),tN=256,tP=e[6];function
tQ(a){return al(tP,tI,a)}var
b9=E.caml_make_vect(h(g[21],tQ,tO,tN),0),tR=bk(k0);dd(function(b){var
a=bZ(b,0);J(b9,a)[1+a]=1;return 0},tR);var
tS=bk(k1);dd(function(b){var
a=bZ(b,0);J(b9,a)[1+a]=2;return 0},tS);var
tT=bk(k2);dd(function(b){var
a=bZ(b,0);J(b9,a)[1+a]=4;return 0},tT);var
tU=bk(k3);dd(function(b){var
a=bZ(b,0);J(b9,a)[1+a]=8;return 0},tU);var
tV=bk(k4);dd(function(b){var
a=bZ(b,0);J(b9,a)[1+a]=16;return 0},tV);var
tW=bk(k5);dd(function(b){var
a=bZ(b,0);J(b9,a)[1+a]=32;return 0},tW);function
id(b){var
a=bZ(b,0);return 1===J(b9,a)[1+a]?1:0}function
k6(b){var
a=bZ(b,0);return 32===J(b9,a)[1+a]?1:0}function
ff(a,d,c){try{var
e=b(a,c);return e}catch(a){a=q(a);if(a===df)return b(d,c);throw a}}function
ah(f,e,d){var
a=b(f,d),g=a[1],c=b(e,a[2]);return[0,[0,g,c[1]],c[2]]}function
cK(a,c){try{var
d=b(a,c),f=d[1],e=cK(a,d[2]),g=[0,[0,f,e[1]],e[2]];return g}catch(a){a=q(a);if(a===df)return[0,0,c];throw a}}function
am(e,d,c){var
a=b(e,c),f=a[2];return[0,b(d,a[1]),f]}function
tX(f,d,c){try{var
h=b(d,c);return h}catch(c){c=q(c);if(c===df){var
g=a(e[17],f,tY);return b(e[3],g)}throw c}}function
tZ(a,c,b){function
d(a){return[0,a[1],a[2]]}function
e(a){return a[2]}function
f(c){return tX(b,a,c)}function
g(a){return ah(c,f,a)}function
h(a){return am(g,e,a)}function
i(a){return cK(h,a)}function
j(b){return ah(a,i,b)}return function(a){return am(j,d,a)}}function
ie(d,c){try{var
a=b(d,c),e=[0,[0,a[1],0],a[2]];return e}catch(a){a=q(a);if(a===df)return[0,0,c];throw a}}function
dY(d,a){if(a){var
c=a[1],e=a[2];if(b(d,c))return[0,c,e];throw df}throw df}function
bu(a){function
b(b){return ab(b,a)}return function(a){return dY(b,a)}}function
fg(b,a,d){if(0<b)var
e=function(a){return[0,a[1],a[2]]},f=b-1|0,g=function(b){return fg(f,a,b)},h=function(b){return ah(a,g,b)},c=function(a){return am(h,e,a)};else
var
c=function(b){return cK(a,b)};return c(d)}var
cL=b(bN[16],0);function
t0(f){try{var
j=b(e[68],f),d=j}catch(c){c=q(c);if(c[1]!==t1)throw c;var
h=a(e[17],t2,f),d=b(e[3],h)}function
c(f){try{var
a=c([0,b(e[72],d),f]);return a}catch(a){a=q(a);if(a===k7)return b(g[9],f);throw a}}var
i=c(0);b(e[82],d);return i}function
fh(b){var
c=t0(b);return a(aW[7],t3,c)}function
cM(f,d){var
c=b(e[49],f);a(e[55],c,d);return b(e[65],c)}function
t4(d,a){var
c=a;for(;;)try{var
e=b(d,c);return e}catch(a){a=q(a);if(a[1]===fb){var
c=c+1|0;continue}throw a}}var
ig=[bc,t5,bb(0)];aF(1013,[0,al,bs,e7,dV,fa,bk,h9,h_,u,av,G,a5,ib,aM,Q,bl,fd,D,dX,aL,ic,fe,cJ,h$,ia,aX,de,dW,e$,e9,e_,h8,e8,bt,am,ah,bu,cK,dY,ie,id,ff,k6,fg,tZ,cL,fh,cM,function(d,c,a){var
e=dt(d,0);if(-1===e)return t4(c,a);if(0===e)throw ig;return function(e,c){var
a=c;for(;;)try{var
f=b(e,a);return f}catch(b){b=q(b);if(b[1]===fb){if(a===d)throw ig;var
a=a+1|0;continue}throw b}}(c,a)},ig],"Micromega_plugin__Sos_lib");var
ih=[bc,t6,bb(0)];function
ii(c){var
e=a(d[9],t8,t7),f=b(d[15],c);if(a(d[27],f,e))return ii(a(d[6],t9,c))-1|0;var
g=b(d[15],c);return a(d[30],g,t_)?ii(a(d[9],c,t$))+1|0:0}function
dZ(j,c){if(a(d[26],c,ua))return ub;var
h=b(d[15],c),f=ii(h),k=e7(-f|0),l=a(d[6],k,h),m=a(d[1],l,uc),n=e7(j),o=a(d[6],n,m),p=b(d[23],o);if(0===f)var
i=ud;else
var
u=b(e[22],f),i=a(e[17],uh,u);var
q=bk(b(d[40],p)),r=fa(b(g[6],q)),s=a(e[17],r,i),t=a(d[27],c,ue)?uf:ug;return a(e[17],t,s)}function
dg(g,f,e,d){var
c=g,a=f,b=d;for(;;){if(a){var
i=a[2],j=h(e,a[1],c,b),c=c+1|0,a=i,b=j;continue}return b}}function
d0(h,g,f){var
c=h,b=f;for(;;){var
e=c[2],d=c[1];if(e<d)return b;var
c=[0,d+1|0,e],b=a(g,d,b);continue}}function
dh(f,e,c){return a(d[26],e,ui)?c:b(G(f,e),c)}function
bv(b,a){return bl(b[2],a,uj)}function
ij(c,a){var
d=a[2],e=a[1];return[0,e,D(function(e,d,a){return dh(d,b(c,a),e)},u,d)]}function
k8(a){return typeof
a[2]==="number"?1:0}function
fi(a){return[0,a,u]}function
k9(c,b){var
e=b[1];if(a(d[26],c,ul))return fi(e);var
f=b[2];return[0,e,aL(function(b){return a(d[6],c,b)},f)]}function
um(a){var
c=b(g[1],a),d=Q(1,c);return[0,c,F(g[26],G,d,a,u)]}function
k_(a){var
b=aL(d[3],a[2]);return[0,a[1],b]}function
k$(d,a){var
c=a[1][2],e=a[2];return[0,c,D(function(c,a,e){var
f=a[2];return a[1]===d?b(G(f,e),c):c},u,e)]}function
uq(a){return 0}function
ur(b,a){return b+a|0}function
fj(a,b){return aM(ur,uq,a,b)}function
la(b,a){return bl(a,b,0)}function
us(a){var
b=1;return D(function(c,b,e){var
a=ab(b,u),d=a?c:a;return d},b,a)}function
cN(b){return a(d[26],b,uu)?u:a5(u,b)}function
lb(b,c){return a(d[26],b,uv)?u:aL(function(c){return a(d[6],b,c)},c)}function
d1(a){return aL(d[3],a)}function
cO(c,b){function
e(b){return a(d[26],b,uw)}return aM(d[1],e,c,b)}function
ik(b,a){return cO(b,d1(a))}function
bO(c,e){return D(function(g,f,c){var
h=a(d[26],c,ux)?u:ab(f,u)?aL(function(b){return a(d[6],c,b)},e):D(function(h,g,e){var
i=a(d[6],c,e);return b(G(fj(f,g),i),h)},u,e);return cO(h,g)},u,c)}function
fk(b,a){if(0===a)return cN(uy);if(1===a)return b;var
d=fk(b,a/2|0),c=bO(d,d);return 1===(a%2|0)?bO(b,c):c}function
fl(b){var
c=0;return D(function(f,d,g){var
b=0,c=D(function(b,c,a){return a+b|0},b,d);return a(e[6],c,f)},c,b)}function
il(a){var
b=0;return dX(function(b,c){var
a=fe(b);return function(b){return h$(a,b)}},a,b)}function
fm(b,a){var
c=a[1],d=b[1],e=cs(d,c),h=a[2],i=b[2];if(e)var
f=e;else
var
g=ab(d,c),f=g?E.caml_greaterthan(i,h):g;return f}function
lc(c){if(ab(c,u))return uA;var
d=0,f=aX(fm,cJ(c));function
i(c,j){var
d=c[2],f=c[1];if(1===d)var
g=f;else
var
h=b(e[22],d),i=a(e[17],uz,h),g=a(e[17],f,i);return[0,g,j]}var
j=h(g[21],i,f,d);return a(aW[7],uB,j)}function
ld(g){var
c=g[2],f=g[1];if(ab(c,u))return b(d[40],f);if(a(d[26],f,uC))return lc(c);var
h=lc(c),i=a(e[17],uD,h),j=b(d[40],f);return a(e[17],j,i)}function
uE(f){if(ab(f,u))return uF;var
j=cJ(f),k=aX(function(o,n){var
i=n[1],j=o[1],h=ab(j,i);if(h)return h;var
m=aX(fm,cJ(i)),b=aX(fm,cJ(j)),a=m;for(;;){if(a){if(b){var
c=a[1],d=b[1],k=a[2],l=b[2],e=fm(d,c);if(e)var
f=e;else{var
g=ab(d,c);if(g){var
b=l,a=k;continue}var
f=g}return f}return 0}return 1}},j);function
l(g,f){var
c=f[2],h=f[1];if(a(d[27],c,uH)){var
i=ld([0,b(d[3],c),h]),j=a(e[17],uI,i);return a(e[17],g,j)}var
k=ld([0,c,h]),l=a(e[17],uJ,k);return a(e[17],g,l)}var
c=h(g[20],l,uG,k),m=h(aW[4],c,0,3),i=h(aW[4],c,3,cX(c)-3|0),n=nw(m,uL)?i:a(e[17],uN,i),o=a(e[17],n,uK);return a(e[17],uM,o)}function
bP(a){if(typeof
a==="number")return u;else
switch(a[0]){case
0:return cN(a[1]);case
1:return a5(a5(a[1],1),ut);case
2:return d1(bP(a[1]));case
3:var
b=a[1],f=b[1],g=bP(b[2]);return cO(bP(f),g);case
4:var
c=a[1],h=c[1],i=bP(c[2]);return ik(bP(h),i);case
5:var
d=a[1],j=d[1],k=bP(d[2]);return bO(bP(j),k);default:var
e=a[1],l=e[2];return fk(bP(e[1]),l)}}function
le(b){var
c=Q(1,b[1]);function
d(a){return bv(b,a)}var
f=20;function
h(a){return dZ(f,a)}function
i(a){return al(h,d,a)}var
j=a(g[17],i,c),k=a(aW[7],uP,j);return a(e[17],k,uO)}function
lf(b){var
c=bk(b),d=a(g[17],bu,c);return bt(function(c,b){function
d(b){return a(e[17],b[1],b[2])}function
f(a){return ah(c,b,a)}return function(a){return am(f,d,a)}},d)}function
fn(a){function
b(a){return a[1][2]}function
c(a){return dY(id,a)}function
d(a){return cK(c,a)}var
e=lf(a);function
f(a){return dY(id,a)}function
g(a){return cK(f,a)}function
h(a){return ah(g,e,a)}function
i(a){return ah(h,d,a)}return function(a){return am(i,b,a)}}function
lg(a){return dY(k6,a)}var
u1=d[43];function
u2(a){return al(u1,fa,a)}var
u3=1;function
u4(a){return fg(u3,lg,a)}function
lh(a){return am(u4,u2,a)}function
u5(c){var
e=e7(b(g[1],c)),f=fa(c),h=b(d[43],f);return a(d[9],h,e)}var
u6=1;function
u7(a){return fg(u6,lg,a)}function
u8(a){return am(u7,u5,a)}function
u9(c){var
b=c[2],e=c[1];if(b)if(!b[2])return a(d[1],e,b[1]);return e}function
u_(a){return a[2]}var
va=bu(u$);function
vb(a){return ah(va,u8,a)}function
vc(a){return am(vb,u_,a)}function
vd(a){return ie(vc,a)}function
ve(a){return ah(lh,vd,a)}function
vf(a){return am(ve,u9,a)}function
li(a){function
b(a){return a[2]}var
c=bu(vg);function
e(b){return ah(c,a,b)}function
f(a){return am(e,b,a)}function
g(b){return ff(f,a,b)}function
h(a){return a[2]}var
i=d[3];function
j(a){return al(i,h,a)}var
k=bu(vh);function
l(b){return ah(k,a,b)}function
m(a){return am(l,j,a)}return function(a){return ff(m,g,a)}}function
vi(a){return a[2]}var
vj=li(lh),vl=bu(vk),vn=bu(vm);function
vo(a){return ff(vn,vl,a)}function
vp(a){return ah(vo,vj,a)}function
vq(a){return am(vp,vi,a)}function
vr(c){var
b=c[2],e=c[1];if(b)if(!b[2]){var
f=a(d[14],vs,b[1]);return a(d[6],e,f)}return e}function
vt(a){return ie(vq,a)}var
vu=li(vf);function
vv(a){return ah(vu,vt,a)}function
lj(a){return am(vv,vr,a)}fn(vx);fn(vy);fn(vz);function
vA(a){return vB}fn(vC);lf(vD);function
vE(a){return a[1]}function
vF(a){return al(um,vE,a)}var
vH=bu(vG),vJ=bu(vI);function
vK(a){return ah(vJ,vH,a)}function
vL(a){return ah(vK,vA,a)}function
vM(a){return[0,a[1],a[2]]}function
vN(a){return a[2]}var
vP=bu(vO);function
vQ(a){return ah(vP,lj,a)}function
vR(a){return am(vQ,vN,a)}function
vS(a){return cK(vR,a)}function
vT(a){return ah(lj,vS,a)}function
vU(a){return am(vT,vM,a)}function
vV(a){return ah(vU,vL,a)}function
im(d){var
a=am(vV,vF,bk(d)),c=a[1];return 0===a[2]?c:b(e[3],vw)}function
lk(b,a){return D(function(b,c,a){return e_(e8(a),b)},a,b)}function
ll(e,c){return D(function(e,g,c){var
f=b(d[15],c);return a(d[38],e,f)},c,e)}function
lm(c){function
e(g){var
e=a(d[6],c,g),f=b(d[23],e);return a(d[9],f,c)}return function(a){return ij(e,a)}}function
lo(o,n){function
W(a){return[0,1,a]}var
X=[0,[0,1,n],a(g[17],W,o)];function
Y(b){function
c(a){return-a|0}var
d=a(g[17],c,b);return a(e[26],d,b)}var
Z=a(g[17],Y,X),l=b(g[1],o)+1|0,p=2*(b(g[1],n)+1|0)|0,f=(p+l|0)-1|0,_=d0([0,1,l],function(a){return G([0,p+a|0,a+1|0],v_)},u),$=dg(1,Z,function(b,a){function
c(c,b){return G([0,b,a],[0,c])}var
d=1;return function(a){return dg(d,b,c,a)}},_),U=Q(1,l);function
V(g){var
a=D(function(c,a,d){var
e=a[1];return a[2]===g?b(G(e,d),c):c},u,$);return[0,[0,f,f],D(function(d,a,c){return b(G([0,a,a],c),d)},u,a)]}var
i=a(g[17],V,U);if(a(d[26],ln,uk))var
m=fi(f);else
var
q=Q(1,f),r=function(a){return G(a,ln)},m=[0,f,h(g[21],r,q,u)];var
c=h(bN[14],0,vX,vW),M=h(aW[4],c,0,cX(c)-6|0),j=a(e[17],M,vY),N=a(bN[4],cL,vZ),s=b(g[1],i)-1|0,t=b(g[5],i)[1][1],v=Q(1,b(g[1],i));function
w(q,p,o){var
c=b(e[22],q-1|0),f=a(e[17],c,uQ),d=0,i=p[2],j=dX(function(b,e,a){var
c=b[2],d=b[1];return c<d?a:[0,[0,[0,d,c],e],a]},i,d);function
k(a){return a[1]}var
l=aX(function(a,b){return dW(k,a,b)},j);function
m(c,g){var
d=c[1],h=c[2],i=d[2],j=d[1],k=a(e[17],uS,g),l=dZ(20,h),m=a(e[17],l,k),n=a(e[17],uT,m),o=b(e[22],i),p=a(e[17],o,n),q=a(e[17],uU,p),r=b(e[22],j),s=a(e[17],r,q);return a(e[17],f,s)}var
n=h(g[21],m,l,uR);return a(e[17],n,o)}var
x=F(g[26],w,v,i,uV),y=le(m),z=a(e[17],y,x),A=a(e[17],uW,z),B=b(e[22],t),C=a(e[17],B,A),E=a(e[17],uX,C),H=a(e[17],uY,E),I=b(e[22],s),J=a(e[17],I,H),K=a(e[17],uZ,J),L=a(e[17],v0,K);cM(c,a(e[17],u0,L));cM(N,io);var
O=a(e[17],j,v4),P=a(e[17],v1,O),R=a(e[17],c,P),S=a(e[17],v2,R),T=a(e[17],cL,S),k=jo(a(e[17],v3,T));im(fh(j));du(c);du(j);if(1!==k)if(2!==k)return 0===k?1:b(e[3],v9);return 0}function
lp(b){if(b){var
c=b[2],d=b[1];return lo(c,d)?c:a(e[26],c,[0,d,0])}throw[0,aV,v$]}function
wa(b,a){return h9(3,lp,[0,b,a])}function
ip(b,c){return a(d[26],b,wb)?0:aL(function(c){return a(d[6],b,c)},c)}function
d2(c,b){function
e(b){return a(d[26],b,wc)}return aM(d[1],e,c,b)}function
lq(e,c){return D(function(h,g,f){var
c=b(fd(e),g),i=a(d[6],c,f);return a(d[1],h,i)},wd,c)}function
lr(k){return function(t){var
i=u,f=t;for(;;){if(f){var
m=f[2],c=f[1];if(av(c)){var
f=m;continue}var
j=ib(c)[1];if(ab(j,k))var
l=b(ic(j),c),h=av(l)?b(e[3],we):ib(l)[1];else
var
h=j;var
n=b(fd(c),h),p=b(ic(h),c),q=ip(a(d[9],wf,n),p),o=function(g,h,i){return function(c){var
e=bl(c,h,wg);if(a(d[26],e,wh))return c;var
f=b(d[3],e);return d2(c,ip(a(d[9],f,i),g))}}(c,h,n),r=a(g[17],o,m),s=aL(o,i),i=b(G(h,q),s),f=r;continue}var
v=0;return[0,de(D(function(c,f,b){var
d=ia(fe(b),[0,k,0]);return a(e[26],d,c)},v,i)),i]}}}function
iq(c){var
g=c[1],f=g[1];if(g[2]!==f)return b(e[3],wj);function
i(l,g){var
c=l;for(;;){if(k8(g))return 0;var
h=bv(g,[0,c,c]);if(a(d[27],h,wk))return b(e[3],wl);if(a(d[26],h,wm)){if(k8(k$(c,g))){var
c=c+1|0;continue}return b(e[3],wn)}var
j=k$(c,g),k=ij(function(b){return a(d[9],b,h)},j);return[0,[0,h,k],i(c+1|0,[0,[0,f,f],d0([0,c+1|0,f],function(b){function
e(c){var
e=bv(k,c),f=bv(j,b),h=a(d[6],f,e),i=bv(g,[0,b,c]),l=a(d[4],i,h),m=[0,b,c];return function(a){return dh(m,l,a)}}var
h=[0,c+1|0,f];return function(a){return d0(h,e,a)}},u)])]}}return i(1,c)}function
ls(b){if(0===b)return[0,wo,b];function
f(e){var
b=e[2],f=e[1],g=b[2],h=D(function(b,c,a){return e9(b,h8(a))},wp,g),i=b[2],j=D(function(b,c,a){return e_(b,e8(a))},wq,i),c=a(d[9],j,h),k=ij(function(b){return a(d[6],c,b)},b),l=a(d[6],c,c);return[0,a(d[9],f,l),k]}var
c=a(g[17],f,b);function
i(a){return a[1]}function
j(a){return al(h8,i,a)}function
k(a){return al(e9,j,a)}var
l=h(g[21],k,c,wr);function
m(a){return a[1]}function
n(a){return al(e8,m,a)}function
o(a){return al(e_,n,a)}var
p=h(g[21],o,c,ws),e=a(d[9],p,l);function
q(b){var
c=b[2];return[0,a(d[6],e,b[1]),c]}var
r=a(g[17],q,c);return[0,a(d[9],wt,e),r]}function
ir(c,d){if(0<=c){if(0===c)return[0,u,0];if(0===d)return[0,u,0];var
f=Q(0,c),h=function(e){var
f=ir(c-e|0,b(g[6],d));function
h(a){return 0===e?a:b(G(b(g[5],d),e),a)}return a(g[17],h,f)},i=a(g[17],h,f);return bt(e[26],i)}return 0}function
is(b,j){var
c=j;for(;;){if(0===b)return[0,[0,cN(bs),[5,bs]],0];if(0<=b){if(c){var
d=c[2],f=c[1],h=f[1],k=f[2],i=fl(h);if(0===i){var
c=d;continue}var
l=is(b-i|0,d),m=function(a){var
b=[10,k,a[2]];return[0,bO(h,a[1]),b]},n=a(g[17],m,l),o=is(b,d);return a(e[26],o,n)}return[0,[0,cN(bs),[5,bs]],0]}return 0}}function
lt(d,c,a){return D(function(a,e,d){return D(function(a,g,f){var
c=fj(e,g),h=bl(a,c,u);return b(G(c,d2(ip(d,f),h)),a)},a,c)},a,d)}function
wT(b){return a(d[26],b,wU)}var
wV=d[1];function
lu(b,c){return a(d[26],b,wW)?u:aL(function(c){return a(d[6],b,c)},c)}function
wY(aC,m,k,r,q){var
aD=0;function
aE(a){return a[1]}var
aF=a(g[17],aE,r),aG=a(e[26],[0,q,k],aF);function
aH(a){return al(h$,il,a)}var
y=h(g[21],aH,aG,aD);if(aC)var
aI=function(a){return fl(a[1])<=m?1:0},aJ=a(g[35],aI,r),f=[0,[0,cN(bs),[5,bs]],aJ];else
var
f=is(m,r);var
aK=b(g[1],f);function
aN(e,d){var
c=ir(m-fl(d)|0,y),f=Q(1,b(g[1],c)),i=a(g[47],c,f);function
j(a){var
b=a[2],c=a[1];return G(c,a5([0,-e|0,-b|0,b],wZ))}return[0,c,h(g[21],j,i,u)]}function
aO(i,e){var
c=ir((m-fl(e[1])|0)/2|0,y),f=Q(1,b(g[1],c)),d=a(g[47],c,f);function
j(e){var
c=e[2],h=e[1];function
f(e,a){var
d=e[2],f=fj(h,e[1]);if(d<c)return a;var
g=c===d?w0:w1,j=bl(a,f,u);return b(G(f,d2(a5([0,i,c,d],g),j)),a)}return a(g[21],f,d)}return[0,c,h(g[21],j,d,u)]}var
aP=Q(1,b(g[1],f)),aQ=h(g[23],aO,aP,f),z=b(g[46],aQ),A=z[1],aR=z[2],aS=Q(1,b(g[1],k)),aT=h(g[23],aN,aS,k),B=b(g[46],aT)[2],s=a(g[17],g[1],A),aU=d1(q),Z=D(function(e,c,a){return b(G(c,a5(wu,b(d[3],a))),e)},u,aU);function
aV(c,b,a){return lt(c[1],b,a)}var
aY=F(g[26],aV,f,aR,Z);function
aZ(c,b,a){return lt(c,b,a)}var
a0=F(g[26],aZ,k,B,aY),a1=0,a2=D(function(b,c,a){return[0,a,b]},a1,a0),C=b(lr(w2),a2),c=C[1],a3=C[2],a4=[0,w3,c];function
a6(a){return G(a,a5(a,w4))}var
t=h(g[21],a6,c,a3);function
a7(j){return D(function(e,c,k){var
h=c[3],i=c[2],f=c[1];if(0<=f){var
g=bl(k,j,w5);if(a(d[26],g,w6))return e;var
l=b(G([0,f,i,h],g),e);return b(G([0,f,h,i],g),l)}return e},u,t)}var
a8=D(function(b,a,c){var
d=a[3],e=a[2];if(0<a[1])if(e===d)return d2(c,b);return b},u,t),n=a(g[17],a7,a4),a9=dg(1,c,function(b,a){var
c=bl(a8,b,w7);return function(b){return dh(a,c,b)}},u),E=[0,b(g[1],c),a9];if(0===c)var
H=fi(0);else{var
N=h(g[21],lk,n,v5),O=lk(E[2],v6),P=function(b){return a(d[6],N,b)},R=function(a){return aL(P,a)},w=a(g[17],R,n),x=k9(O,E),S=h(g[21],ll,w,v7),T=ll(x[2],v8),U=dV(20-(Math.log(b(d[56],S))/on|0)|0),V=dV(20-(Math.log(b(d[56],T))/on|0)|0),W=function(b){return a(d[6],b,U)},X=function(a){return aL(W,a)},o=a(g[17],X,w),Y=k9(V,x),i=h(bN[14],0,wJ,wI),ar=h(aW[4],i,0,cX(i)-6|0),p=a(e[17],ar,wK),as=a(bN[4],cL,wL),_=b(g[1],o)-1|0,$=Q(1,b(g[1],o)),aa=function(p,o,n){var
c=b(e[22],p-1|0),f=a(e[17],c,wv),d=0,i=D(function(b,a,e){var
c=a[3],d=a[2],f=a[1];return c<d?b:[0,[0,[0,f,d,c],e],b]},d,o);function
j(a){return a[1]}var
k=aX(function(a,b){return dW(j,a,b)},i);function
l(d,g){var
c=d[1],h=d[2],i=c[3],j=c[2],k=c[1],l=a(e[17],wx,g),m=dZ(20,h),n=a(e[17],m,l),o=a(e[17],wy,n),p=b(e[22],i),q=a(e[17],p,o),r=a(e[17],wz,q),s=b(e[22],j),t=a(e[17],s,r),u=a(e[17],wA,t),v=b(e[22],k),w=a(e[17],v,u);return a(e[17],f,w)}var
m=h(g[21],l,k,ww);return a(e[17],m,n)},ab=F(g[26],aa,$,o,wB),ac=le(Y),ad=a(e[17],ac,ab),ae=a(e[17],wC,ad),af=a(g[17],e[22],s),ag=a(aW[7],wD,af),ah=a(e[17],ag,ae),ai=a(e[17],wE,ah),aj=b(e[22],aK),ak=a(e[17],aj,ai),am=a(e[17],wF,ak),an=b(e[22],_),ao=a(e[17],an,am),ap=a(e[17],wG,ao),aq=a(e[17],wM,ap);cM(i,a(e[17],wH,aq));cM(as,io);var
at=a(e[17],p,wQ),au=a(e[17],wN,at),aw=a(e[17],i,au),ax=a(e[17],wO,aw),ay=a(e[17],cL,ax),j=jo(a(e[17],wP,ay)),az=im(fh(p));du(i);du(p);if(1===j)var
l=0;else
if(2===j)var
l=0;else
if(3===j)var
l=1;else
if(0===j)var
l=1;else{var
aA=b(e[22],j),aB=a(e[17],wS,aA);b(e[3],aB);var
l=1}if(!l)b(e[3],wR);var
H=az}function
I(i){var
c=b(lm(i),H),l=lu(wX,a(g[7],n,0));function
j(b,d){var
e=a(g[7],n,b);return aM(wV,wT,lu(bv(c,b),e),d)}var
k=d0([0,1,c[1]],j,l),d=Q(1,b(g[1],s)),e=a(g[47],s,d);function
f(a){var
c=a[1],d=a[2];return[0,[0,c,c],D(function(c,a,e){var
f=a[3],g=a[2];return a[1]===d?b(G([0,g,f],e),c):c},u,k)]}var
h=a(g[17],f,e);return[0,c,a(g[17],iq,h)]}if(0===c)var
v=I(bs);else
var
bp=Q(5,66),bq=a(g[17],dV,bp),br=Q(1,31),bt=a(g[17],d[47],br),v=h_(I,a(e[26],bt,bq));var
J=v[1],a_=v[2],a$=a5(w9,w8),ba=Q(1,J[1]);function
bb(b){var
d=bv(J,b);return G(a(g[7],c,b-1|0),d)}var
K=h(g[21],bb,ba,a$),bc=D(function(d,c,a){return b(G(c,lq(K,a)),d)},K,t);function
bd(a){return D(function(c,b,a){return dh(b,lq(bc,a),c)},u,a)}function
be(c){function
d(d){var
e=d[2],f=d[1],i=Q(1,b(g[1],c));function
j(b,d){var
f=bv(e,b);return dh(a(g[7],c,b-1|0),f,d)}return[0,f,h(g[21],j,i,u)]}return b(g[17],d)}var
bf=h(g[23],be,A,a_),L=a(g[17],bd,B);function
bg(b,a){return[0,b,a]}var
bh=h(g[23],bg,f,bf);function
bi(a){return 0!==a[2]?1:0}var
M=a(g[35],bi,bh),bj=d1(q);function
bk(b,a){var
c=bO(b,a);return function(a){return cO(c,a)}}var
bm=F(g[26],bk,L,k,bj);function
bn(a){var
c=a[2],d=a[1][1];function
b(a){var
b=a[2],c=a[1],d=lb(c,bO(b,b));return function(a){return cO(d,a)}}var
e=bO(d,h(g[21],b,c,u));return function(a){return cO(e,a)}}if(av(h(g[21],bn,M,bm))){var
bo=function(a){return[0,a[1][2],a[2]]};return[0,L,a(g[17],bo,M)]}throw ih}function
it(a){var
b=cJ(a);function
c(a){return a[1]}return aX(function(a,b){return dW(c,a,b)},b)}function
lv(a){if(ab(a,u))return[0,bs];var
b=it(a),c=0;function
d(a,d){var
b=a[2],c=a[1],e=1===b?[1,c]:[6,[0,[1,c],b]];return[0,e,d]}var
e=h(g[21],d,b,c);return bt(function(b,a){return[5,[0,b,a]]},e)}function
w_(e){var
b=e[2],c=e[1];return ab(c,u)?[0,b]:a(d[26],b,bs)?lv(c):[5,[0,[0,b],lv(c)]]}function
lw(b){if(ab(b,u))return 0;var
c=cJ(b),d=aX(function(C,B){var
o=B[1],p=C[1];if(ab(o,u))return 1;if(ab(p,u))return 0;var
k=it(p),l=it(o),s=0;function
t(a){return a[2]}function
v(b,a){return b+a|0}function
w(a){return al(v,t,a)}var
m=h(g[21],w,k,s),x=0;function
y(a){return a[2]}function
z(b,a){return b+a|0}function
A(a){return al(z,y,a)}var
n=h(g[21],A,l,x);if(m<n)return 0;if(n<m)return 1;var
b=k,a=l;for(;;){if(b){if(a){var
c=a[1],d=c[2],e=c[1],f=b[1],i=f[2],j=f[1],q=a[2],r=b[2];if(cs(j,e))return 1;if(cs(e,j))return 0;if(cs(i,d))return 0;if(cs(d,i))return 1;var
b=r,a=q;continue}return 0}return a?1:1}},c),e=a(g[17],w_,d);return bt(function(b,a){return[3,[0,b,a]]},e)}function
w$(a){var
b=a[1];return[10,[5,b],[6,lw(a[2])]]}function
xa(b){var
c=b[2],d=b[1];if(0===c)return d;var
e=a(g[17],w$,c);return[10,d,bt(function(b,a){return[9,b,a]},e)]}function
lx(b){if(0===b)return xb;var
c=0;function
d(c,d){var
f=lx(ia(b,[0,c,0]));function
h(a){return[0,c,a]}var
i=a(g[17],h,f);return a(e[26],i,d)}return h(g[21],d,b,c)}function
iu(d,c){return D(function(f,e,c){return b(G(a(g[38],e,d),c),f)},u,c)}aF(1015,[0,us,d1,bO,fk,cN,bP,lw,xa,uE,wY,function(c){var
n=il(c),o=il(c),K=fe(c);function
L(b){function
c(a){return la(a,b)}return a(g[17],c,o)}var
t=a(g[17],L,K);function
M(f){var
b=0;return(D(function(c,b,g){var
d=la(f,b);return a(e[6],d,c)},b,c)+1|0)/2|0}var
N=a(g[17],M,o);function
O(a){var
b=Q(0,a);function
c(b,a){return[0,b,a]}return function(a){return e$(c,b,a)}}var
P=h(g[21],O,N,wi),H=[0,b(g[5],t),0],I=b(g[6],t),s=h(g[21],wa,I,H),J=h9(b(g[1],s),lp,s);function
R(b){function
c(a){return 2*a|0}return lo(J,a(g[17],c,b))}var
S=a(g[35],R,P),T=b(g[9],S);function
U(a){function
c(d,c,a){return 0===c?a:b(G(d,c),a)}return F(g[26],c,o,a,u)}var
i=a(g[17],U,T),q=b(g[1],i),aD=lx(n);function
aE(d){var
e=a(g[47],n,d);return av(ik(c,D(function(d,c,a){return b(G(iu(e,c),a),d)},u,c)))}var
aF=a(g[35],aE,aD),aG=Q(1,b(g[1],i)),v=a(g[47],i,aG),aH=e$(function(b,a){return[0,[0,b[1],a[1]],[0,b[2],a[2]]]},v,v);function
aI(b){var
a=b[2];return a[1]<=a[2]?1:0}var
aJ=a(g[35],aI,aH);function
aK(b){var
c=b[2],d=b[1],e=c[2],f=c[1],h=d[2],i=d[1];function
j(b){var
c=iu(a(g[47],n,b),h);return[0,[0,iu(a(g[47],n,b),i),c],[0,f,e]]}return a(g[17],j,aF)}var
aN=a(g[17],aK,aJ),w=bt(e[26],aN);function
aO(a){return a[1]}var
aP=de(a(g[17],aO,w));function
aQ(b){function
c(a){return ab(a[1],b)}return a(g[35],c,w)}var
aR=a(g[17],aQ,aP);function
aS(a){return a[2]}var
aT=b(g[17],aS);function
aU(a){return al(de,aT,a)}var
aV=a(g[17],aU,aR);function
aY(c,d){if(c){var
f=c[2],h=c[1];if(f){var
i=function(a){var
c=a5(h,xA);return b(G(a,xB),c)},j=a(g[17],i,f);return a(e[26],j,d)}return d}throw ih}var
aZ=h(g[21],aY,aV,0),a0=D(function(d,c,a){return b(G(c,a5(xC,a)),d)},u,c),a1=dg(1,i,function(f,a){function
c(g,d,c){var
e=fj(f,g);if(d<a)return c;var
h=a===d?xD:xE,i=bl(c,e,u);return b(G(e,b(G([0,a,d],h),i)),c)}var
d=1;return function(a){return dg(d,i,c,a)}},a0),a2=0,a3=D(function(b,c,a){return[0,a,b]},a2,a1),a4=a(e[26],a3,aZ),x=b(lr(xF),a4),j=x[1],a6=x[2];function
a7(a){return G(a,a5(a,xG))}var
y=h(g[21],a7,j,a6),a8=[0,xH,j],a9=Q(1,q);function
a_(a){return b(fd(y),[0,a,a])}var
a$=bt(d2,a(g[17],a_,a9));function
ba(i){return[0,[0,q,q],D(function(f,e,j){var
g=e[2],h=e[1],c=bl(j,i,xI);if(a(d[26],c,xJ))return f;var
k=b(G([0,h,g],c),f);return b(G([0,g,h],c),k)},u,y)]}var
f=a(g[17],ba,a8),bb=dg(1,j,function(b,a){var
c=bl(a$,b,xK);return function(b){return dh(a,c,b)}},u),z=[0,b(g[1],j),bb];if(0===j)var
A=fi(0);else{var
k=h(bN[14],0,xq,xp),as=h(aW[4],k,0,cX(k)-6|0),p=a(e[17],as,xr),at=a(bN[4],cL,xs),ac=b(g[1],f)-1|0,ad=b(g[5],f)[1][1],ae=Q(1,b(g[1],f)),af=function(q,p,o){var
c=b(e[22],q-1|0),f=a(e[17],c,xe),d=0,i=p[2],j=dX(function(b,e,a){var
c=b[2],d=b[1];return c<d?a:[0,[0,[0,d,c],e],a]},i,d);function
k(a){return a[1]}var
l=aX(function(a,b){return dW(k,a,b)},j);function
m(c,g){var
d=c[1],h=c[2],i=d[2],j=d[1],k=a(e[17],xg,g),l=dZ(20,h),m=a(e[17],l,k),n=a(e[17],xh,m),o=b(e[22],i),p=a(e[17],o,n),q=a(e[17],xi,p),r=b(e[22],j),s=a(e[17],r,q);return a(e[17],f,s)}var
n=h(g[21],m,l,xf);return a(e[17],n,o)},ag=F(g[26],af,ae,f,xj),V=Q(1,z[1]),W=function(a){return bv(z,a)},X=20,Y=function(a){return dZ(X,a)},Z=function(a){return al(Y,W,a)},_=a(g[17],Z,V),$=a(aW[7],xd,_),aa=a(e[17],$,xc),ah=a(e[17],aa,ag),ai=a(e[17],xk,ah),aj=b(e[22],ad),ak=a(e[17],aj,ai),am=a(e[17],xl,ak),an=a(e[17],xm,am),ao=b(e[22],ac),ap=a(e[17],ao,an),aq=a(e[17],xn,ap),ar=a(e[17],xt,aq);cM(k,a(e[17],xo,ar));cM(at,io);var
au=a(e[17],p,xx),aw=a(e[17],xu,au),ax=a(e[17],k,aw),ay=a(e[17],xv,ax),az=a(e[17],cL,ay),l=jo(a(e[17],xw,az)),aA=im(fh(p));du(k);du(p);if(1===l)var
m=0;else
if(2===l)var
m=0;else
if(3===l)var
m=1;else
if(0===l)var
m=1;else{var
aB=b(e[22],l),aC=a(e[17],xz,aB);b(e[3],aC);var
m=1}if(!m)b(e[3],xy);var
A=aA}function
bc(c){var
l=b(lm(c),A),h=k_(a(g[7],f,0));function
i(n,m){var
o=a(g[7],f,n),p=bv(l,n),h=o[1],i=h[2],j=h[1];if(a(d[26],p,un))var
c=[0,[0,j,i],u];else
var
q=o[2],c=[0,[0,j,i],aL(function(b){return a(d[6],p,b)},q)];var
k=c[1];if(E.caml_notequal(k,m[1]))return b(e[3],uo);var
r=m[2],s=c[2];function
t(b){return a(d[26],b,up)}return[0,k,aM(d[1],t,s,r)]}return ls(iq(d0([0,1,l[1]],i,h)))}if(0===j)var
r=ls(iq(k_(a(g[7],f,0))));else
var
bg=Q(5,66),bh=a(g[17],dV,bg),bi=Q(1,31),bj=a(g[17],d[47],bi),r=h_(bc,a(e[26],bj,bh));var
B=r[1],bd=r[2];function
be(c){var
d=c[1],e=c[2][2];return[0,d,D(function(e,d,c){return b(G(a(g[7],i,d-1|0),c),e)},u,e)]}var
C=a(g[17],be,bd);function
bf(a){var
b=a[1],c=fk(a[2],2);return bO(cN(b),c)}if(av(ik(lb(B,bt(cO,a(g[17],bf,C))),c)))return[0,B,C];throw ih}],"Micromega_plugin__Sos");var
iv=[0,e[8]],xM=d[2],xN=d[7],Y=b(cC[1],[0,x[1]]),xL=0;function
ly(a){var
b=0;function
c(c,b,a){return b+a|0}return h(Y[13],c,a,b)}function
lz(d){try{var
e=b(Y[24],d),f=e[1],i=e[2],g=a(Y[26],f,d),j=g[3];if(b(Y[2],g[1]))if(b(Y[2],j))var
h=[0,[0,f,i]],c=1;else
var
c=0;else
var
c=0;if(!c)var
h=0;return h}catch(a){a=q(a);if(a===K)return 0;throw a}}function
fo(e,a){function
c(b,a){var
c=a[2],d=a[1];return 1===c?h(n[1],b,xO,d):F(n[1],b,xP,d,c)}function
d(b,a){if(a){var
e=a[2],f=a[1];return e?V(n[1],b,xQ,c,f,d,e):c(b,f)}return 0}return d(e,b(Y[19],a))}var
di=Y[1];function
lA(a){var
b=0;function
c(c,b,a){return a+b|0}return h(Y[13],c,a,b)}function
fp(c,b){var
d=lA(c),e=lA(b);return a(x[2],d,e)?h(Y[10],x[1],c,b):a(x[1],d,e)}function
fq(a){return ab(a,Y[1])}function
fr(a){return h(Y[4],a,1,Y[1])}function
lB(b){var
a=lz(b);return a?1===a[1][2]?1:0:0}function
iw(c){var
a=lz(c);if(a){var
b=a[1],d=b[1];return 1===b[2]?[0,d]:0}return 0}function
lC(a){if(fq(a))return 0;try{var
b=function(c,a,b){var
d=a/2|0;if(0===(a%2|0))return h(Y[4],c,d,b);throw K},c=[0,h(Y[13],b,a,di)];return c}catch(a){a=q(a);if(a===K)return 0;throw a}}function
ix(c,b){try{var
d=a(Y[27],c,b);return d}catch(a){a=q(a);if(a===K)return 0;throw a}}function
fs(b,a){function
c(b,c,a){var
d=ix(b,a)+c|0;return h(Y[4],b,d,a)}return h(Y[13],c,b,a)}function
lD(c,b){var
f=e[8];function
g(f,d,b){var
g=E.caml_div(ix(f,c),d);return a(e[5],b,g)}var
d=h(Y[13],g,b,f),i=Y[1];function
j(c,f,a){var
e=f-hc(ix(c,b),d)|0;return 0===e?a:h(Y[4],c,e,a)}return[0,h(Y[13],j,c,i),d]}function
lE(b){var
c=t[1];function
d(c,d,b){return a(t[4],c,b)}return h(Y[13],d,b,c)}var
lF=Y[13],C=b(cC[1],[0,fp]),lG=C[8],xR=C[1],xS=C[2],xT=C[3],xU=C[4],xV=C[5],xW=C[6],xX=C[7],xY=C[10],xZ=C[11],x0=C[12],x1=C[13],x2=C[14],x3=C[15],x4=C[16],x5=C[17],x6=C[18],x7=C[19],x8=C[20],x9=C[21],x_=C[22],x$=C[23],ya=C[24],yb=C[25],yc=C[26],yd=C[27],ye=C[28],yf=C[29],yg=C[30],yh=C[31],yi=C[32],yj=C[33],yk=C[34],yl=C[35],ym=C[36],yn=C[37],yo=C[38],dj=[0,xR,xS,xT,xU,xV,xW,xX,lG,xY,xZ,x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x_,x$,ya,yb,yc,yd,ye,yf,yg,yh,yi,yj,yk,yl,ym,yn,yo,function(e){return b(lG,function(f,b,a){if(b){var
c=b[1];if(a)return h(e,f,c,a[1]);var
d=c}else{if(!a)return 0;var
d=a[1]}return[0,d]})}];function
yp(e,g){var
c=g[2],f=g[1];if(fq(f)){if(a(d[32],yq,c))return 0;var
j=b(d[40],c);return h(n[1],e,yr,j)}if(0===c[0]){var
i=c[1]+1|0;if(!(2<i>>>0))switch(i){case
0:return F(n[1],e,yt,fo,f);case
1:return 0;default:return fo(e,f)}}var
k=b(d[40],c);return U(n[1],e,ys,k,fo,f)}var
an=b(cC[1],[0,fp]);function
lH(c,b){function
d(b,a){return F(n[1],c,yu,yp,[0,b,a])}return a(an[12],d,b)}function
lI(c,b){try{var
d=a(an[27],c,b);return d}catch(a){a=q(a);if(a===K)return yv;throw a}}function
yw(a){var
b=an[1],c=fr(a);return h(an[4],c,yx,b)}function
d3(a){return h(an[4],di,a,an[1])}function
d4(e,f,c){if(0===b(d[25],f))return c;var
g=a(xM,lI(e,c),f);return 0===b(d[25],g)?a(an[7],e,c):h(an[4],e,g,c)}function
lJ(b,a){function
c(c,b,a){return d4(c,b,a)}return h(an[13],c,b,a)}function
lK(c,i){var
e=an[1];function
f(k,c,j){if(0===b(d[25],c))var
e=d3(yy);else
var
f=an[1],g=function(e,d,b){var
f=a(xN,c,d),g=fs(k,e);return h(an[4],g,f,b)},e=h(an[13],g,i,f);return lJ(e,j)}return h(an[13],f,c,e)}function
yz(c){function
e(a){return b(d[3],a)}return a(an[33],e,c)}var
lL=an[13],ft=[bc,yC,bb(0)];function
lM(a){return 2===a[2]?1:0}function
fu(a){switch(a){case
0:return d[26];case
1:return d[30];default:return d[28]}}function
d5(a){switch(a){case
0:return yD;case
1:return yE;default:return yF}}function
yG(c,a){var
e=a[2],f=a[1],g=b(d[40],a[3]),h=d5(e);return V(n[1],c,yH,cG,f,h,g)}function
iy(c,b){switch(c){case
2:if(2<=b)return 2;var
a=0;break;case
1:var
a=0;break;default:var
a=1}if(!a)if(0!==b)return 1;return 0}function
fv(c,a){switch(c){case
0:var
d=a,b=1;break;case
1:if(1===a)return 1;var
b=0;break;default:var
b=0}if(!b){if(0!==a)return 2;var
d=c}return d}var
fw=b(cC[1],[0,fp]),fx=b(cC[1],[0,x[1]]),fy=[0,fw[1]],fz=[0,fx[1]],dk=[0,0];function
yI(c){if(c<dk[1]){var
d=a(n[4],yJ,c);return b(e[3],d)}dk[1]=c+1|0;return 0}function
yK(a){return dk[1]}function
dl(b){try{var
d=a(fw[27],b,fy[1]);return d}catch(a){a=q(a);if(a===K){var
c=dk[1];fy[1]=h(fw[4],b,c,fy[1]);fz[1]=h(fx[4],c,b,fz[1]);dk[1]++;return c}throw a}}function
bw(b){return a(fx[27],b,fz[1])}function
yL(a){fy[1]=fw[1];fz[1]=fx[1];dk[1]=0;dl(di);return 0}dl(di);function
lN(a){return H(dl(fr(a)),yM,P)}function
yN(a){return H(dl(a),yO,P)}function
fA(a){return h(lL,function(c,b,a){return H(dl(c),b,a)},a,P)}function
d6(a){var
b=d3(yP);return X(function(c,b,a){return d4(bw(b),a,c)},b,a)}function
iz(a,c){var
d=[0,b(a,yR)];return X(function(g,f,e){var
i=bw(f),c=[0,b(a,yQ)],d=h(lF,function(d,c,a){var
e=b(A[3],c);return[4,[6,[1,b(A[1],d)],e],a]},i,c);return[2,[4,[0,b(a,e)],d],g]},d,c)}function
lO(c,b){try{var
a=fo(c,bw(b));return a}catch(a){a=q(a);if(a===K)return h(n[1],c,yS,b);throw a}}function
d7(b,a){return e0(lO,b,a)}function
cP(a){return 0===b(d[25],a)?P:H(0,a,P)}function
yT(a){return e5(function(c,d){var
a=bw(c),b=lB(a);return b?b:fq(a)},a)}function
yU(e){var
b=h3(e),c=b[1],f=c[2],g=c[1];if(b7(b[2]))if(a(d[28],f,yV))return iw(bw(g));return 0}function
fB(g,f){var
i=d6(f),c=fr(g),b=d3(yA),d=[0,d3(yB),b];function
e(f,e,d){var
g=d[2],h=d[1],i=lD(f,c),j=i[2],k=i[1];if(0===j)return[0,h,d4(f,e,g)];var
b=di,a=j-1|0;for(;;){if(0===a)return[0,d4(fs(k,b),e,h),g];var
b=fs(b,c),a=a-1|0;continue}}var
a=h(an[13],e,i,d),j=a[1],k=fA(a[2]);return[0,fA(j),k]}function
lP(b,a){return kO(fB(b,a)[1])}function
lQ(f,c){var
a=0;return X(function(a,h,g){if(b(f,g)){var
d=iw(bw(h));if(d){var
e=d[1];return lP(e,c)?[0,e,a]:a}return a}return a},a,c)}function
lR(c,b){var
a=lQ(c,b);return a?[0,h(g[20],e[5],a[1],a[2])]:0}function
bQ(b,a){var
c=d6(a);return fA(lK(d6(b),c))}function
iA(b,a){return aK(b,a)}function
yW(a){return X(function(c,b,a){var
d=cP(a);return iA(bQ(lN(b),d),c)},P,a)}function
iB(b){var
c=t[1];return X(function(c,b,e){var
d=lE(bw(b));return a(t[7],d,c)},c,b)}function
yX(b){var
c=t[1];return X(function(c,b,d){return a(t[4],b,c)},c,b)}function
yY(b){var
c=0;return X(function(c,b,f){var
d=ly(bw(b));return a(e[6],c,d)},c,b)}function
yZ(e,b,c){var
f=t[1];function
i(c,b){var
d=iB(b[1]);return a(t[7],c,d)}var
d=h(g[20],i,f,c);function
j(b,f){function
c(a){return F(n[1],b,y0,a,e)}return a(t[13],c,d)}F(n[1],b,y1,j,d);function
k(c,a){var
d=a[1],e=d5(a[2]);return V(n[1],b,y2,c,d7,d,e)}a(g[16],k,c);return a(n[1],b,y3)}function
y4(a){var
b=dj[1];return X(function(a,d,e){var
b=bw(d),c=lC(b);return c?h(dj[4],c[1],b,a):a},b,a)}function
aN(e,c){if(typeof
c==="number")return a(n[1],e,y5);else
switch(c[0]){case
0:return U(n[1],e,y6,aN,c[2],c[1]);case
1:return h(n[1],e,y7,c[1]);case
2:return h(n[1],e,y8,c[1]);case
3:var
f=b(d[40],c[1]);return h(n[1],e,y9,f);case
4:var
g=d6(c[1]);return F(n[1],e,y_,lH,g);case
5:var
i=c[2],j=d6(c[1]);return V(n[1],e,y$,lH,j,aN,i);case
6:var
k=c[2],l=b(o[33],c[1]);return U(n[1],e,za,aN,k,l);case
7:return V(n[1],e,zb,aN,c[1],aN,c[2]);case
8:return V(n[1],e,zc,aN,c[1],aN,c[2]);default:return F(n[1],e,zd,aN,c[1])}}function
fC(c,b){if(typeof
b==="number")return a(n[1],c,ze);else
switch(b[0]){case
0:return hd(n[1],c,zf,b[1],aN,b[2],fC,b[3]);case
1:var
f=b[5],g=b[4],h=b[3],i=b[2],j=b[1],k=function(a,b){return kv(zg,fC,a,b)};return nx(n[1],c,zh,j,aN,i,cG,h,aN,g,k,f);default:var
d=b[6],e=b[5];return J1(n[1],c,zi,b[1],b[4],e,d,b[2],e,b[3],d,fC,b[7])}}function
fD(c){var
b=c;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
b=b[2];continue;case
1:return zk;case
2:return zl;case
3:return b[1];case
5:var
b=b[2];continue;case
6:var
e=[1,b[1]],f=fD(b[2]);return a(d[9],f,e);case
9:var
b=b[1];continue;case
4:break;default:var
g=b[1],h=fD(b[2]),i=fD(g);return a(d[1],i,h)}return zj}}function
dm(f){var
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
g=b[1],h=dm(b[2]),i=dm(g);return a(e[6],i,h);default:var
c=0}if(c){var
b=d;continue}return-1}}function
d8(b){if(typeof
b==="number")return-1;else
switch(b[0]){case
0:var
c=b[2],d=b[1],f=d8(b[3]),i=dm(c),j=a(e[6],i,f);return a(e[6],d,j);case
1:var
k=b[5],l=b[2],m=b[1],n=dm(b[4]),o=dm(l),p=a(e[6],o,n),q=a(e[6],m,p),r=function(c,b){var
d=d8(b);return a(e[6],c,d)};return h(g[20],r,q,k);default:var
s=b[3],t=b[2],u=b[1],v=d8(b[7]),w=a(e[6],u,t),x=a(e[6],w,s);return a(e[6],x,v)}}function
bR(c,n){var
b=n;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
b=b[2];continue;case
5:var
o=b[1],d=bR(c,b[2]);return[0,d[1],d[2],[5,o,d[3]]];case
6:var
f=bR(c,b[2]),g=f[2];return[0,[0,[0,g,f[3]],f[1]],g+1|0,[2,g]];case
7:var
p=b[2],h=bR(c,b[1]),q=h[3],r=h[1],i=bR(h[2],p),s=i[2],t=[7,q,i[3]];return[0,a(e[26],i[1],r),s,t];case
8:var
u=b[2],j=bR(c,b[1]),v=j[3],w=j[1],k=bR(j[2],u),x=k[2],y=[8,v,k[3]];return[0,a(e[26],k[1],w),x,y];case
9:var
l=bR(c,b[1]),m=l[2];return[0,[0,[0,m,l[3]],l[1]],m+1|0,[2,m]]}return[0,0,c,b]}}function
iC(c,a){if(typeof
a!=="number"&&9===a[0]){var
b=bR(c,a[1]);return[0,b[1],b[2],[9,b[3]]]}return bR(c,a)}function
lS(b){var
a=b;for(;;){if(typeof
a!=="number"&&9===a[0]){var
a=a[1];continue}return a}}function
dn(e){var
b=e;for(;;){if(typeof
b==="number")var
c=0;else
switch(b[0]){case
0:var
b=b[2];continue;case
9:var
d=b[1],c=1;break;case
1:case
2:return a(t[4],b[1],t[1]);case
5:case
6:var
d=b[2],c=1;break;case
7:case
8:var
f=b[1],g=dn(b[2]),h=dn(f);return a(t[7],h,g);default:var
c=0}if(c){var
b=d;continue}return t[1]}}function
fE(d,p){var
c=p;for(;;)if(typeof
c==="number")return[0,d,0];else
switch(c[0]){case
0:var
f=c[2],l=c[1];if(typeof
f!=="number"&&6===f[0])if(typeof
c[3]==="number"){var
c=[0,l,f[2],0];continue}var
q=c[3],i=iC(d,f),r=i[3],s=i[1],m=fE(i[2],q),t=m[1],u=[0,l,r,m[2]],v=function(b,a){return[0,a[1],[9,a[2]],b]};return[0,t,h(g[20],v,u,s)];case
1:var
w=c[5],x=c[4],y=c[3],z=c[1],j=iC(d,lS(c[2])),A=j[3],B=j[2],C=j[1],k=iC(B,lS(x)),D=k[3],E=k[2],F=k[1],G=function(a){return fE(E,a)},H=a(g[17],G,w),n=b(g[46],H),I=n[2],J=n[1],K=a(e[26],F,C),L=[1,z,A,y,D,I],M=function(b,a){return[0,a[1],[9,a[2]],b]},N=h(g[20],M,L,K);return[0,h(g[20],e[6],0,J),N];default:var
O=c[6],P=c[5],Q=c[4],R=c[3],S=c[2],T=c[1],o=fE(d,c[7]);return[0,o[1],[2,T,S,R,Q,P,O,o[2]]]}}function
lT(d,c){function
e(c){if(typeof
c==="number")return[0,0,t[1]];else
switch(c[0]){case
0:var
n=c[3],i=c[2],f=c[1];if(typeof
n==="number"){var
x=dn(i);return[0,c,a(t[4],f,x)]}var
o=e(n),j=o[2],p=o[1];if(a(t[3],f,j)){var
y=dn(i),z=a(t[7],y,j);return[0,[0,f,i,p],a(t[4],f,z)]}return[0,p,j];case
1:var
q=c[4],r=c[2],s=c[1],A=c[3],B=a(g[17],e,c[5]),u=b(g[46],B),C=u[1],D=h(g[20],t[7],t[1],u[2]),E=dn(q),F=dn(r),G=a(t[7],F,E),H=a(t[7],G,D);return[0,[1,s,r,A,q,C],a(t[4],s,H)];default:var
k=c[3],l=c[2],m=c[1],I=c[6],J=c[5],K=c[4],v=e(c[7]),d=v[2],w=v[1];if(!a(t[3],m,d))if(!a(t[3],l,d))if(!a(t[3],k,d))return[0,w,d];var
L=a(t[4],k,d),M=a(t[4],l,L);return[0,[2,m,l,k,K,J,I,w],a(t[4],m,M)]}}return fE(d,e(c)[1])}function
lU(a){if(typeof
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
fF(f,e,c,b){var
g=b[2],h=c[2],d=a(f,c[1],b[1]);return 0===d?a(e,h,g):d}function
cQ(h,g){var
c=h,b=g;for(;;){if(typeof
c==="number"){if(typeof
b==="number")return 0}else
switch(c[0]){case
0:if(typeof
b!=="number"&&0===b[0]){var
e=b[1],f=c[1],k=b[2],l=c[2];if(nw(f,e)){var
c=l,b=k;continue}return a(aW[33],f,e)}break;case
1:if(typeof
b!=="number"&&1===b[0])return a(x[1],c[1],b[1]);break;case
2:if(typeof
b!=="number"&&2===b[0])return a(x[1],c[1],b[1]);break;case
3:if(typeof
b!=="number"&&3===b[0])return a(d[37],c[1],b[1]);break;case
4:if(typeof
b!=="number"&&4===b[0])return a(h1,c[1],b[1]);break;case
5:if(typeof
b!=="number"&&5===b[0])return fF(h1,cQ,[0,c[1],c[2]],[0,b[1],b[2]]);break;case
6:if(typeof
b!=="number"&&6===b[0])return fF(o[23],cQ,[0,c[1],c[2]],[0,b[1],b[2]]);break;case
7:if(typeof
b!=="number"&&7===b[0])return fF(cQ,cQ,[0,c[1],c[2]],[0,b[1],b[2]]);break;case
8:if(typeof
b!=="number"&&7===b[0])return fF(cQ,cQ,[0,c[1],c[2]],[0,b[1],b[2]]);break;default:if(typeof
b!=="number"&&9===b[0]){var
c=c[1],b=b[1];continue}}var
i=lU(b),j=lU(c);return a(x[1],j,i)}}function
fG(b,a){if(typeof
b==="number")var
c=a;else{if(typeof
a!=="number")return[8,b,a];var
c=b}return c}function
dp(e,c){if(typeof
c!=="number")switch(c[0]){case
0:var
g=c[1];return[0,g,dp(e,c[2])];case
5:var
h=c[2];return[5,at(e,c[1]),h]}var
f=b(d[25],e)+1|0;if(2<f>>>0)throw[0,aV,zm];switch(f){case
0:return[5,cP(e),c];case
1:return 0;default:return a(d[32],zn,e)?c:[7,[3,e],c]}}function
lV(b,a){var
c=br(b),d=c[1];return b7(c[2])?dp(d,a):[5,b,a]}function
lW(b,a){if(typeof
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
d=0;if(!d)return[7,b,a]}return dp(e,f)}return 0}var
cR=b(cC[1],[0,cQ]);function
iD(a){var
b=0;function
c(c,b,a){return fG(lV(b,c),a)}return h(cR[13],c,a,b)}function
dq(e){var
b=e;for(;;){if(typeof
b!=="number")switch(b[0]){case
0:var
b=b[2];continue;case
5:var
g=b[1],i=dq(b[2]),j=function(a){return bQ(g,a)};return a(cR[33],j,i);case
7:var
l=b[2],m=dq(b[1]),d=dq(l),c=iD(m),n=iD(d);if(typeof
c!=="number"&&3===c[0]){var
p=c[1],q=function(a){return at(p,a)};return a(cR[33],q,d)}var
o=cP(zq);return a(cR[6],[7,c,n],o);case
8:var
r=b[1],s=dq(b[2]),t=dq(r),u=function(e,b,a){if(b){var
c=b[1];if(a)return[0,iA(c,a[1])];var
d=c}else{if(!a)return 0;var
d=a[1]}return[0,d]};return h(cR[8],u,t,s);case
6:case
9:var
k=cP(zp);return a(cR[6],b,k)}var
f=cP(zo);return a(cR[6],b,f)}}function
zr(c,b){var
d=0;return X(function(e,d,b){return fG(dp(b,a(s[27],d,c)),e)},d,b)}function
lX(a){if(a){var
b=a[1],c=lX(a[2]);return h(n[4],zs,b,c)}return zt}function
lY(k,g,j,a){function
c(q){var
a=q;for(;;)if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
a=a[2];continue;case
3:return[5,b(g,a[1])];case
4:return[1,b(k,iz(g,a[1]))];case
5:var
r=a[2],s=b(k,iz(g,a[1]));return[2,s,c(r)];case
7:var
t=a[1],u=c(a[2]);return[3,c(t),u];case
8:var
v=a[1],w=c(a[2]);return[4,c(v),w];case
1:case
2:var
l=a[1],f=0,d=j;for(;;){if(d){var
m=d[2];if(l!==d[1]){var
f=f+1|0,d=m;continue}var
i=f}else
var
o=lX(j),p=h(n[4],zu,l,o),i=b(e[3],p);return[0,b(A[4],i)]}default:return b(e[3],zv)}}return c(a)}function
fH(c,a){return lY(eO,function(a){var
c=ak(a);return b(A[2],c)},c,a)}function
d9(d,c){if(typeof
c==="number")return 0;else
switch(c[0]){case
0:var
f=c[3],e=c[2],h=c[1];if(typeof
e!=="number"&&9===e[0]){var
j=e[1],k=d9([0,h,d],f);return[1,fH(d,j),k]}var
i=d9([0,h,d],f);return[0,fH(d,e),i];case
1:var
l=c[5],m=c[4],n=c[2],o=[0,c[1],d],p=function(a){return d9(o,a)},q=a(g[17],p,l),r=fH(d,m);return[2,fH(d,n),r,q];default:var
s=c[4],t=d9([0,c[1],[0,c[2],[0,c[3],d]]],c[7]);return[3,b(A[1],s),t]}}function
zw(b,a){return d9(b,lT(1+d8(a)|0,a)[2])}function
bx(f,u){var
c=u;for(;;)if(typeof
c==="number")return[0,P,1];else
switch(c[0]){case
0:var
c=c[2];continue;case
3:var
g=c[1],h=a(d[37],g,zx),v=0===h?1:1===h?2:b(e[3],zy);return[0,H(0,g,P),v];case
4:var
i=c[1];return[0,bQ(i,i),1];case
5:var
j=c[2],k=c[1],l=bx(f,j),m=l[2],o=l[1];if(0===m)return[0,bQ(k,o),0];var
w=d5(m);J2(n[1],e[28],zz,cG,k,aN,j,cG,o,w);return b(e[3],zA);case
6:var
x=c[1],p=bx(f,c[2]),y=p[2];return[0,c$([1,x],p[1]),y];case
7:var
z=c[2],q=bx(f,c[1]),A=q[2],B=q[1],r=bx(f,z),C=r[1],D=iy(A,r[2]);return[0,bQ(B,C),D];case
8:var
E=c[2],s=bx(f,c[1]),F=s[2],G=s[1],t=bx(f,E),I=t[1],J=fv(F,t[2]);return[0,iA(G,I),J];case
9:var
c=c[1];continue;default:return b(f,c[1])}}function
zC(o,m){var
d=o,c=m;for(;;)if(typeof
c==="number")return b(e[3],zD);else
switch(c[0]){case
0:var
j=c[3],p=c[2],q=c[1],k=bx(function(c){return function(b){return a(s[27],b,c)}}(d),p),f=k[2],g=k[1],i=br(g),l=i[1],r=b7(i[2])?1-a(fu(f),l,zB):0;if(r)return 1;if(0===j){var
t=d5(f);U(n[1],e[28],zE,d7,g,t);return 0}var
d=h(s[4],q,[0,g,f],d),c=j;continue;case
1:var
u=c[4],v=c[2];bx(function(b){return a(s[27],b,d)},v);bx(function(b){return a(s[27],b,d)},u);return b(e[3],zF);default:return b(e[3],zG)}}function
zH(b,a){return[0,a[1],[0,b,a[2]]]}function
iE(c,a){var
b=a[1],d=a[2],e=b[1],f=d5(b[2]);return hd(n[1],c,zI,d7,e,f,aN,d)}function
zJ(c,b){var
d=h(n[1],c,zK,iE);return a(g[15],d,b)}var
lZ=[bc,zL,bb(0)],zM=[0,[0,P,0],0];function
zN(a){return[0,[0,cP(a),1],[3,a]]}function
zO(c){var
a=c[1],e=c[2],f=a[2],g=a[1];return[0,[0,H(0,b(d[3],a[3]),g),f],e]}function
fI(b,a){var
c=a[1],d=b[1],e=c[2],f=c[1],g=d[2],h=d[1],i=lW(b[2],a[2]),j=iy(g,e);return[0,[0,bQ(h,f),j],i]}function
fJ(b,a){var
c=a[1],d=b[1],e=c[2],f=c[1],g=d[2],h=d[1],i=fG(b[2],a[2]),j=fv(g,e);return[0,[0,aK(h,f),j],i]}function
fK(b,g){var
e=g[2],h=g[1],c=h[2],f=h[1];if(0===c){var
k=lV(b,e);return[0,[0,bQ(b,f),c],k]}var
i=br(b),j=i[1];if(b7(i[2]))if(a(d[28],j,zP)){var
l=dp(j,e);return[0,[0,bQ(b,f),c],l]}U(n[2],zQ,d7,b,iE,[0,[0,f,c],e]);throw lZ}function
zR(g){var
h=g[2],i=g[1],j=i[2],k=i[1],l=br(k),f=l[1],c=h6(l[2]);if(!a(o[24],o[2],c))if(!a(d[26],f,zS)){var
p=o[2],q=bh(f);if(a(o[24],q,p)){var
m=a(d[9],f,[1,c]),n=b(d[22],m);if(a(d[26],m,n))return 0;switch(j){case
0:return[0,[0,[0,H(0,zT,P),0],[6,c,h]]];case
1:return[0,[0,[0,H(0,n,c$([1,c],k)),j],[6,c,h]]];default:return b(e[3],zU)}}}return 0}function
zV(f){var
c=br(f),a=c[1];if(b7(c[2])){var
e=b(d[25],a),g=0===e?zW:1===e?[0,1,2,[3,a]]:[0,0,2,[3,b(d[3],a)]];return[0,g]}return 0}function
iF(d,c){var
b=zV(c);if(b)return[0,b[1]];try{var
k=function(a){return dS(c,a[1][1])},f=a(g[33],k,d),l=[0,[0,1,f[1][2],f[2]]];return l}catch(b){b=q(b);if(b===K){var
h=b8(c);try{var
i=function(a){return dS(h,a[1][1])},e=a(g[33],i,d),j=[0,[0,0,e[1][2],e[2]]];return j}catch(a){a=q(a);if(a===K)return 0;throw a}}throw b}}function
iG(f,a){var
b=a[2],c=a[1],d=c[2],e=c[1];return f?[0,[0,e,d],b]:[0,[0,b8(e),d],b]}function
fL(q,I,H,G){var
J=G[1],K=I[1],j=[0,[0,K[1],K[2]],I[2]],i=[0,[0,J[1],J[2]],G[2]];for(;;){var
m=i[2],s=i[1],d=s[2],k=s[1],f=j[2],t=j[1],a=t[2],c=t[1],l=fB(H,c)[1],g=fB(H,k)[1];if(b7(g))var
h=[0,[0,[0,k,d],m]];else
if(0===a){if(0===d){var
M=fK(b8(g),[0,[0,c,a],f]),u=fJ(fK(l,[0,[0,k,d],m]),M),v=u[1],j=[0,[0,c,a],f],i=[0,[0,v[1],v[2]],u[2]];continue}var
w=iF(q,l);if(w){var
n=w[1],x=n[1],N=iG(x,[0,[0,l,n[2]],n[3]]),O=x?b8(g):g,P=fK(O,[0,[0,c,a],f]),y=fJ(fI(N,[0,[0,k,d],m]),P),z=y[1],j=[0,[0,c,a],f],i=[0,[0,z[1],z[2]],y[2]];continue}var
h=0}else
if(0===d)var
h=b(e[3],zX);else{var
A=iF(q,l),B=iF(q,g);if(A)if(B){var
o=B[1],C=o[1],p=A[1],D=p[1],Q=o[3],R=o[2],S=p[3],T=p[2];if(D!==C){var
U=fI(iG(C,[0,[0,g,R],Q]),[0,[0,c,a],f]),E=fJ(fI(iG(D,[0,[0,l,T],S]),[0,[0,k,d],m]),U),F=E[1],j=[0,[0,c,a],f],i=[0,[0,F[1],F[2]],E[2]];continue}var
h=0,r=1}else
var
r=0;else
var
r=0;if(!r)var
h=0}if(h){var
L=h[1],V=L[1];return[0,[0,V,iD(dq(L[2]))]]}return 0}}function
fM(e,c){var
b=c[1],f=b[2],g=b[1];function
h(b){if(e){var
c=a(d[26],b,zY);return c?c:a(d[26],b,zZ)}return 1}return 0===f?lR(h,g):0}function
z0(a){var
f=1,b=bM(function(a){return fM(f,a)},a),c=b[1],g=b[2];if(c){var
d=c[1],h=d[2],i=d[1],e=eS(function(b){return fL(a,h,i,b)},g);return e?e[1]:a}return a}function
z1(a){return eU(function(e){var
f=1,b=bM(function(a){return fM(f,a)},e),c=b[1],g=b[2];if(c){var
d=c[1],h=d[2],i=d[1];return eS(function(b){return fL(a,h,i,b)},g)}return 0},a)}var
ao=[0,lZ,zH,zO,iE,zJ,zM,zN,fI,fJ,fK,zR,fL,z1,z0,function(c,b){function
d(a){return fM(c,a)}return kB(d,function(d,c){var
e=c[1],f=e[1],g=d[1],h=c[2],i=e[2],j=d[2],k=iB(f);return a(t[3],g,k)?fL(b,j,g,[0,[0,f,i],h]):0},b)},fM],S=[0,fD,dm,d8,lT,aN,fC,fG,dp,lW,zw,lY,zr,bx,zC],T=[0,[0,yL,yI,yK,bw,dl],fA,lN,iz,yN,yW,iB,yU,yT,lP,cP,lR,lQ,bQ,fB,y4,yX,yY,lO,d7,yZ],by=[0,d3,yw,lJ,lK,yz,lI,lL,d4],fN=[0,lF,ly,di,fq,fr,fs,lC,lB,iw,lD,fp,lE];aF(1016,[0,xL,iv,fN,dj,by,fu,fv,lM,ft,T,S,yG,iy,ao],"Micromega_plugin__Polynomial");function
iH(c,b){var
a=b[2];return a?c===a[1]?1:0:0}function
d_(b,a){var
c=a[1]<=b?1:0,d=c?1-iH(b,a):c;return d}function
fO(a){return[0,a,0]}function
l0(b,a){return[0,a[1],[0,b]]}function
z3(c,b){return e5(function(e,b){return a(d[28],b,z4)?0:d_(e,c)},b)}function
z5(c,b){try{var
a=br(b),d=a[1],e=z3(c,a[2])?[0,d]:0;return e}catch(a){a=q(a);if(a===K)return 0;throw a}}function
iI(f,d,c){try{var
g=a(s[27],d,c);return g}catch(a){a=q(a);if(a===K)return b(e[3],f);throw a}}function
l1(j,f,c){var
k=iI(Ag,f,j),i=au(c,k);if(a(d[26],i,z_))var
g=b(e[3],z$);else
var
l=a(d[9],Aa,i),g=at(l,H(f,Ac,H(c,Ab,k)));var
o=a(s[7],f,j);function
m(b){var
e=au(c,b);return a(d[26],e,Ad)?b:e4(e,g,Af,H(c,Ae,b))}var
n=a(s[33],m,o);return h(s[4],c,g,n)}function
Ai(b,a){return 0}var
Aj=[0,b(s[10],Ai)];b(hT[1],Aj);function
iJ(D,g,k,C){var
c=C;for(;;){if(!D){var
G=da(a(s[27],g,c));if(a(d[30],G,Al))return[0,c,0]}var
r=iI(z9,g,c),t=z5(k,r);if(t)var
f=[0,[0,t[1]]];else{var
p=br(r)[2];for(;;){var
q=a4(p);if(q){var
l=q[1],m=l[1],x=l[3];if(a(d[27],l[2],z6)){if(d_(m,k)){var
p=x;continue}var
j=[0,m,-1]}else
var
j=[0,m,1]}else
var
j=b(e[3],z7);var
n=j[1],z=j[2],A=a(s[7],g,c),y=0,v=a(s[39],k[1],A),w=function(p,q){return function(e,g,c){if(iH(e,k))return c;var
j=au(q,g),m=a(d[6],[0,p],j);if(a(d[27],m,z8)){var
n=da(g),o=a(d[9],n,j),f=b(d[15],o);if(c){var
h=c[1],i=h[2],l=h[1];return a(d[27],i,f)?c:a(d[27],f,i)?[0,[0,e,f]]:cs(l,e)?c:[0,[0,e,f]]}return[0,[0,e,f]]}return c}}(z,n),o=h(s[13],w,v,y);if(o)var
u=o[1],f=[1,u[1],n,u[2]];else
var
f=[0,[1,n]];break}}if(0===f[0]){var
i=f[1];if(typeof
i==="number")throw[0,l2,Ak];else{if(0===i[0])return[0,c,i];var
E=i[1],B=da(a(s[27],g,c)),F=a(d[30],B,Ah)?c:l1(c,g,E);return[0,F,i]}}var
c=l1(c,f[1],f[2]);continue}}function
iK(d,e,c){var
b=X(function(d,c,b){try{var
f=aK(at(b,a(s[27],c,e)),d);return f}catch(a){a=q(a);if(a===K)return aK(H(c,b,P),d);throw a}},P,c);return h(s[4],d,b,e)}function
l3(j,b,i,h,g){var
f=iJ(j,b,h,iK(b,g,i)),c=f[2],e=f[1];return typeof
c==="number"?[1,e,0]:0===c[0]?a(d[30],c[1],Am)?[1,e,0]:[0,H(b,Aq,H(0,Ap,at(Ao,iI(An,b,e))))]:[1,e,[0,c[1]]]}function
fP(m,l){var
c=0,a=m,g=s[1],f=l,e=0;for(;;){if(f){var
j=f[2],i=f[1];switch(i[2]){case
0:var
n=i[1],k=H(0,b(d[3],i[3]),n),o=at(Ar,k),p=h(s[4],a+1|0,[0,c,0],g),q=h(s[4],a,[0,c,1],p),c=c+1|0,r=[0,[0,a,k],[0,[0,a+1|0,o],e]],a=a+2|0,g=q,f=j,e=r;continue;case
1:var
t=i[1],u=[0,[0,a,H(0,b(d[3],i[3]),t)],e],v=h(s[4],a,[0,c,1],g),c=c+1|0,a=a+1|0,g=v,f=j,e=u;continue;default:throw ft}}return[0,a,g,e]}}function
l4(c,a){function
b(b,d,a){return d_(b,c)?a:H(b,da(d),a)}return h(s[13],b,a,P)}function
fQ(E,D,f,C){var
h=D,g=C;for(;;){var
n=H(0,As,l4(f,g));if(h){var
t=h[1],u=t[2],A=h[2],B=t[1],c=A,o=e6(n,u),e=[0,B,u],b=0;for(;;){var
j=e[2],k=e[1];if(c){var
p=c[2],q=c[1],l=q[2],r=q[1],s=e6(n,l);if(a(d[29],s,o)){var
c=p,o=s,e=[0,r,l],b=[0,[0,k,j],b];continue}var
c=p,e=[0,k,j],b=[0,[0,r,l],b];continue}var
m=[0,[0,[0,k,j],b]];break}}else
var
m=0;if(m){var
v=m[1],w=v[2],x=v[1],y=x[1],F=x[2],i=l3(E,y,F,l0(y,f),g);if(0===i[0])return[1,i[1]];var
z=i[1],G=i[2];if(w){var
h=w,g=z;continue}return[0,[0,f,z,G]]}return[0,[0,f,g,0]]}}function
l5(g){var
c=b(T[1][3],0),e=fP(c,g),h=e[3],j=e[2],i=s[1],f=fQ(0,h,fO(c),i);if(0===f[0])return 0;var
k=f[1];return[0,db(X(function(g,f,c){var
e=a(s[27],f,j),h=e[1],i=e[2]?c:b(d[3],c);return H(h,i,g)},P,k))]}function
At(d){try{var
e=t[1],f=function(c,b){var
d=h2(b[1]);return a(t[7],c,d)},i=h(g[20],f,e,d),j=b(t[24],i),c=j}catch(a){a=q(a);if(a!==K)throw a;var
c=0}return 1+c|0}function
l6(a){var
b=At(a),e=fP(b,a)[3],f=s[1],c=fQ(0,e,fO(b),f);if(0===c[0]){var
d=c[1];return[0,l4(d[1],d[2])]}return 0}function
l7(c,j){var
a=b(T[1][3],0),k=fP(a+1|0,j)[3];function
e(f,e){var
a=e[2];if(typeof
a==="number")return 0;else{if(0===a[0]){var
c=a[1],g=f?c:b(d[3],c);return[0,g]}return 0}}var
l=s[1],f=fQ(0,k,fO(a),l);if(0===f[0]){var
g=f[1],h=g[2],i=g[1],m=e(1,iJ(1,a,i,iK(a,h,c)));return[0,[0,e(0,iJ(1,a,i,iK(a,h,b8(c)))),m]]}return 0}function
iL(c){var
e=b(d[22],c);return a(d[4],c,e)}function
l8(i,C,o,r,n,B,h){var
j=h[2],e=h[1],k=br(j),p=k[2],c=iL(k[1]);if(a(d[26],c,Au))return 0;var
t=a(d[9],Aw,Av);if(a(d[27],c,t))var
f=a(d[9],Ax,c),u=b(d[20],f)?a(d[4],f,Ay):b(d[22],f),l=u;else
var
l=AE;function
v(e){var
b=iL(e),f=a(d[4],Az,c);if(a(d[29],b,f)){var
g=a(d[4],AA,c);return a(d[9],b,g)}var
h=a(d[4],AB,b);return a(d[9],h,c)}var
w=[0,v,[0,function(b){return iL(a(d[6],l,b))},0]];function
x(e){return db(X(function(c,a,d){return d_(a,n)?H(a,b(e,d),c):c},P,p))}var
y=a(g[17],x,w);function
z(e){var
c=ao[6];return X(function(h,e,c){try{var
g=a(s[27],e,r),l=g[1],m=g[2]?c:b(d[3],c),n=a(s[27],l,i),o=e2(m),p=a(ao[10],o,n),f=p}catch(b){b=q(b);if(b!==K)throw b;var
j=a(s[27],e,i),k=e2(c),f=a(ao[10],k,j)}return a(ao[9],h,f)},c,e)}var
A=a(g[17],z,y),m=ky(function(h){var
d=b(ao[11],h);if(d){var
f=d[1],g=f[2],c=f[1];if(0===c[2])return[0,[0,e,[0,c,g]]];var
i=H(0,AC,o),j=e6(c[1],i);return a(fu(1),j,AD)?0:[0,[0,e,[0,c,g]]]}return 0},A);return m?[0,m[1]]:[1,[0,e,j]]}function
l9(a){return h3(a)[1][1]}function
l_(j){var
p=b(g[46],j)[1],f=3*b(T[1][3],0)|0,c=fP(f,p),m=c[2],r=c[3],t=c[1],u=a(g[17],ao[3],j),k=[0,0,s[1]];function
l(a,c){var
b=a[1];return[0,b+1|0,h(s[4],b,c,a[2])]}var
w=[0,0],v=h(g[20],l,k,u)[2];function
o(g,r,c,p){w[1]++;if(0===p[0]){var
x=p[1],f=x[2],i=x[1],J=function(c,b,a){return H(c,da(b),a)},u=h(s[13],J,f,P);if(0===(w[1]%2|0))var
M=0,N=function(k,j,b){var
e=[0,k,j];function
h(a){return l8(g,r,u,m,i,f,a)}if(typeof
b==="number"){var
a=h(e);return typeof
a==="number"?0:0===a[0]?[0,a[1]]:[1,a[1]]}else{if(0===b[0])return[0,b[1]];var
d=b[1],c=h(e);return typeof
c==="number"?[1,d]:0===c[0]?[0,c[1]]:[1,d]}},j=h(s[13],N,f,M);else
var
O=0,Q=function(t,s,e){var
c=l8(g,r,u,m,i,f,[0,t,s]);if(typeof
c==="number")return e;if(typeof
e==="number")if(typeof
c==="number")var
h=0;else{if(1===c[0])return[1,c[1]];var
h=0}else
if(0===e[0]){var
l=e[1];if(typeof
c==="number")var
j=1;else
if(0===c[0])var
j=1;else
var
k=e[1],h=1,j=0;if(j){var
n=c[1],o=l[2][2],p=b(S[1],n[2][2]),q=b(S[1],o);return a(d[27],q,p)?[0,l]:[0,n]}}else
if(typeof
c==="number")var
h=0;else{if(1===c[0])return[1,c[1]];var
h=0}if(!h)var
k=c[1];return[0,k]},j=h(s[13],Q,f,O);if(typeof
j==="number")return 0;else{if(0===j[0]){var
y=j[1],z=y[2],A=z[2],B=z[1],C=B[2],D=B[1],V=y[1];if(0===C)return[0,[0,c,[9,A],0]];b(T[1][2],c);var
t=l0(c,i),k=l3(1,c,D,t,f);if(0===k[0])var
E=[1,k[1]];else{var
R=k[2],U=k[1];if(iH(c,t))var
v=[0,t[1],0];else
var
I=a(n[4],z2,c),v=b(e[3],I);var
E=[0,[0,v,U,R]]}var
F=o(h(s[4],c,[0,[0,D,C],[2,c]],g),[0,V],c+1|0,E);return F?[0,[0,c,[9,A],F[1]]]:0}var
W=j[1][2],Y=[0,s[1],c,g,f],l=X(function(f,e,B){if(0!==e)if(!d_(e,i)){var
c=f[2],o=f[4],p=f[3],q=f[1];b(T[1][2],c);var
g=b(T[3],c+1|0),j=l9(g),k=b(T[3],c+2|0),l=l9(k),m=aK(g,b8(k)),n=[0,[0,H(e,AF,b8(m)),0],[2,c]],r=[0,[0,g,1],[2,j]],t=[0,[0,k,1],[2,l]],u=function(a){return h4(e,m,a)},v=a(s[33],u,o),w=function(c){var
f=au(e,c[1][1]);if(a(d[26],f,AG))return c;var
g=e2(b(d[3],f)),h=a(ao[10],g,n);return a(ao[9],h,c)},x=a(s[33],w,p),y=h(s[4],l,t,x),z=h(s[4],j,r,y),A=h(s[4],c,n,z);return[0,h(s[4],e,[0,c,j,l],q),c+3|0,A,v]}return f},Y,W),Z=l[1],G=o(l[3],r,l[2],[0,[0,i,l[4],0]]);if(G){var
_=G[1],$=function(e,a,d){var
b=a[3],c=a[2];return[2,a[1],c,b,e,c,b,d]};return[0,h(s[13],$,Z,_)]}return 0}}var
aa=0,ab=db(p[1]),L=0;return[0,[0,c,X(function(i,e,c){try{var
h=a(s[27],e,m),k=h[2],l=a(s[27],h[1],g)[2],n=k?c:b(d[3],c),o=a(S[8],n,l),f=o}catch(b){b=q(b);if(b!==K)throw b;var
j=a(s[27],e,g)[2],f=a(S[8],c,j)}return a(S[7],i,f)},L,ab),aa]]}var
x=s[1],i=o(v,0,t,fQ(1,r,fO(f),x));return i?[0,i[1]]:0}aF(1018,[0,l7,l6,l5,l_],"Micromega_plugin__Simplex");function
l$(p){var
c=b(bi[25],p),n=[bc,AH,bb(0)],o=[bc,AI,bb(0)];function
r(a,c){try{var
d=b(a,0);b(c,0);return d}catch(a){a=q(a);try{b(c,0)}catch(b){throw a}throw a}}function
s(a){try{var
c=[0,b(eY[3],a)];return c}catch(a){a=q(a);if(a===k7)return 0;if(b(fR[13],a))throw n;throw a}}function
t(c,a){var
d=h(O[34],a,0,1);try{h(O[34],a,0,0);var
e=0===c?4:1;h(O[83],a,e,1);var
f=1,b=f}catch(a){a=q(a);if(a[1]!==O[1])throw a;var
b=0}h(O[34],a,d,0);return b}function
u(a){var
c=h(O[34],a,0,1);try{h(O[34],a,0,0);var
b=h(O[83],a,0,1);return b}catch(b){b=q(b);if(b[1]===O[1]){h(O[34],a,c,0);return 0}throw b}}function
g(d,c,a){return t(d,c)?r(a,function(a){return u(c)}):b(a,0)}function
d(i){var
f=h(O[23],i,AJ,jw),j=b(O[30],f),d=b(c[1],100);function
m(e){for(;;){var
a=s(j);if(a){var
b=a[1];h(c[5],d,b[1],b[2]);continue}return 0}}try{g(0,f,m);b(e[83],j);var
o=h(O[23],i,AM,jw),p=[0,b(O[31],o),1,d];return p}catch(f){f=q(f);if(f===n){b(e[83],j);var
l=h(O[23],i,AK,jw),k=b(O[31],l);g(1,l,function(g){function
f(b,a){return h(eY[1],k,[0,b,a],AL)}a(c[12],f,d);return b(e[52],k)});return[0,k,1,d]}throw f}}function
l(a,i,f){var
d=a[1],j=a[3];if(0===a[2])throw o;var
k=b(O[33],d);h(c[5],j,i,f);return g(1,k,function(a){h(eY[1],d,[0,i,f],AN);return b(e[52],d)})}function
m(b,d){var
e=b[3];if(0===b[2])throw o;return a(c[7],e,d)}function
v(c,e){var
a=[f,function(b){try{var
a=[0,d(c)];return a}catch(a){return 0}}];return function(c){var
d=j(a),g=k===d?a[1]:f===d?b(i[2],a):a;if(g){var
h=g[1];try{var
o=m(h,c);return o}catch(a){a=q(a);if(a===K){var
n=b(e,c);l(h,c,n);return n}throw a}}return b(e,c)}}return[0,d,m,l,v,function(a,o,e){var
c=[f,function(c){try{var
b=[0,d(a)];return b}catch(a){return 0}}];return function(a){var
d=j(c),g=k===d?c[1]:f===d?b(i[2],c):c;if(g){var
h=g[1];if(b(o,a))try{var
p=m(h,a);return p}catch(c){c=q(c);if(c===K){var
n=b(e,a);l(h,a,n);return n}throw c}return b(e,a)}return b(e,a)}}]}aF(1021,[0,l$],"Micromega_plugin__Persistent_cache");function
ma(c,f){var
g=f[2],i=f[1];if(i){var
j=b(d[40],i[1]);h(n[1],c,AO,j)}else
a(e[55],c,AS);a(e[55],c,AP);if(g){var
k=b(d[40],g[1]);return h(n[1],c,AQ,k)}return a(e[55],c,AR)}function
iM(b){var
c=b[1];if(c){var
e=b[2];if(e)return a(d[29],c[1],e[1])?[0,b]:0}return[0,b]}function
iN(c,b){var
f=b[2],g=b[1],h=c[2],i=c[1];function
e(d,c,b){if(c){var
e=c[1];return b?[0,a(d,e,b[1])]:c}return b?b:0}var
j=e(d[39],h,f);return iM([0,e(d[38],i,g),j])}function
fS(c){var
e=c[1];if(e){var
f=c[2];if(f){var
g=f[1],h=b(d[24],e[1]),i=b(d[22],g),j=a(d[4],i,h);return[0,a(d[1],j,AT)]}}return 0}function
mb(f,e){var
b=fS(f),c=fS(e);return b?c?a(d[29],b[1],c[1]):1:0}function
iO(e,b){var
c=e[2],f=e[1];if(f){var
g=f[1];if(c){var
i=c[1],h=a(d[29],g,b);return h?a(d[29],b,i):h}return a(d[29],g,b)}return c?a(d[29],b,c[1]):1}aF(1022,[0,ma,iN,fS,mb,iO,iM],"Micromega_plugin__Itv");var
d$=b(m[25][1],[0,dt]),aY=b(bi[25],[0,dS,kM]),ea=[bc,AU,bb(0)];function
eb(b,a){switch(a[0]){case
0:return h(n[1],b,AV,a[1]);case
1:return hd(n[1],b,AW,a[1],eb,a[2],eb,a[3]);default:return V(n[1],b,AX,eb,a[1],eb,a[2])}}function
AY(b,a){var
c=b[4],d=b[3],f=a[4],g=a[2],h=a[1],i=b[2],j=b[1];if(d===a[3])if(c===f){var
e=iN(j,h);return e?[0,[0,e[1],[2,i,g],d,c]]:0}throw[0,aV,AZ]}function
A0(f,c,e){try{var
d=a(aY[7],e,f),g=AY(c,b(m[3],d));if(g){d[1]=g[1];var
i=0;return i}var
j=b(m[3],d)[2];throw[0,ea,[2,c[2],j]]}catch(a){a=q(a);if(a===K)return h(aY[10],e,f,[0,c]);throw a}}var
fT=[bc,A1,bb(0)];function
iP(d,c,a){var
e=b(m[3],iv);if(b(aY[15],a)<e)return A0(d,c,a);throw fT}function
iQ(i,c){var
j=iM(c[1]);if(j){var
k=j[1],f=k[2],g=k[1],l=a4(i);if(l){var
h=l[1][2],e=function(b){return a(d[9],b,h)};if(1===b(d[25],h))var
n=c[4],o=c[3],p=c[2],q=a(aO[16],e,f),m=[0,[0,a(aO[16],e,g),q],p,o,n];else
var
r=c[3],s=c[4],t=c[2],u=a(aO[16],e,g),m=[0,[0,a(aO[16],e,f),u],t,s,r];return[0,c$(h,i),m]}return iO([0,g,f],A2)?1:0}return 0}function
mc(c){return X(function(c,i,h){var
e=c[2],f=c[1],g=b(d[25],h);if(0===g)throw[0,aV,A4];return 1===g?[0,f,a(m[4],e,1)]:[0,a(m[4],f,1),e]},A3,c)}function
md(a,f){var
b=a[3],c=a[1],g=a[2],d=mc(c),h=d[2],i=d[1],j=[0,f];switch(g){case
0:var
e=[0,[0,b],[0,b]];break;case
1:var
e=[0,[0,b],0];break;default:throw ft}return iQ(c,[0,e,j,h,i])}function
A5(d){var
c=b(aY[1],1000);function
e(b,a){return[0,a,b]}var
f=a(m[22][13],e,d),g=d$[1];function
i(e,d){var
f=d[2],g=d[1],b=md(g,f);if(typeof
b==="number"){if(0===b)throw[0,ea,[0,f]];return e}iP(b[1],b[2],c);var
h=g[1];return X(function(c,b,d){return a(d$[4],b,c)},e,h)}return[0,c,h(m[22][15],i,g,f)]}function
me(a){var
c=a[1],d=0;function
e(d,c,a){return[0,[0,d,b(m[3],c)],a]}return h(aY[14],e,c,d)}function
iR(e,c){var
f=c[2],g=e[2],i=c[1],j=e[1];if(a(d[31],g,A6))if(a(d[31],f,A7)){var
h=a(d[9],A8,f),b=e4(a(d[9],A9,g),j,h,i);return[0,b,mc(b)]}throw[0,aV,A_]}function
A$(i,c){var
g=c[1];function
j(k,r,j){var
a=b(m[3],r),f=j[3],g=j[2],h=j[1],c=au(i,k);if(0===c[0])if(0===c[1])return[0,h,[0,[0,k,a],g],f];function
e(d,b){return b?[0,[0,c,k,[0,[0,[0,b[1]],0],a[2],a[3],a[4]]],d]:d}var
l=a[1],n=l[2],o=l[1];if(1===b(d[25],c)){var
p=e(f,n);return[0,e(h,o),g,p]}var
q=e(f,o);return[0,e(h,n),g,q]}var
e=h(aY[14],j,g,Ba),k=e[3],l=e[2],n=e[1],o=b(aY[15],c[1]),f=b(aY[1],o);function
p(a){return h(aY[10],f,a[1],[0,a[2]])}a(m[22][11],p,l);function
q(e){function
c(g){var
h=g[3],j=g[1],k=e[3],l=e[1],p=g[2],q=e[2],r=h[1],s=b(aO[7],k[1][1]),t=b(aO[7],r[1]),u=b(d[3],j),v=a(d[9],t,u),w=a(d[9],s,l),x=a(d[1],w,v),m=iR([0,q,l],[0,p,b(d[3],j)]),n=m[2],o=[0,[0,[0,x],0],[1,i,k[2],h[2]],n[2],n[1]],c=iQ(m[1],o);if(typeof
c==="number"){if(0===c)throw[0,ea,o[2]];return 0}return iP(c[1],c[2],f)}return a(m[22][11],c,k)}a(m[22][11],q,n);return[0,f,a(d$[6],i,c[2])]}function
Bc(e,r,D,C,c){var
s=au(e,r),f=b(aY[15],c[1]),t=b(aY[1],f),g=c[1];function
h(g,E){var
h=b(m[3],E),c=au(e,g);if(0===c[0])if(0===c[1])var
i=[0,g,h],j=1;else
var
j=0;else
var
j=0;if(!j)var
k=a(d[30],c,Bb)?b(d[3],s):s,l=b(d[15],c),n=iR([0,r,k],[0,g,l]),o=n[2],v=o[2],w=o[1],x=n[1],y=a(d[9],D,k),p=function(b){var
c=a(d[9],b,l);return a(d[1],y,c)},q=h[1],z=q[1],A=a(aO[16],p,q[2]),B=[0,a(aO[16],p,z),A],i=[0,x,[0,B,[1,e,C,h[2]],v,w]];var
u=i[2],f=iQ(i[1],u);if(typeof
f==="number"){if(0===f)throw[0,ea,u[2]];return 0}return iP(f[1],f[2],t)}a(aY[12],h,g);return[0,t,a(d$[6],e,c[2])]}var
I=b(Bd[1],[0,dt]);function
mf(y,x,c){function
f(p,o,z){var
u=[0,Be,P],i=X(function(e,c,b){var
f=e[2],g=e[1];try{var
h=a(I[23],c,y),i=a(d[6],h,b),j=[0,a(d[1],g,i),f];return j}catch(a){a=q(a);if(a===K)return[0,g,H(c,b,f)];throw a}},u,p),r=i[2],f=i[1],s=au(x,r),g=b(m[3],o)[1];function
c(b){var
c=a(d[4],b,f);return a(d[9],c,s)}var
j=g[2],k=g[1],l=b(d[25],s);if(0===l)var
h=iO(g,f)?Bf:b(e[3],Bg);else
if(1===l)var
v=a(aO[16],c,j),h=[0,a(aO[16],c,k),v];else
var
w=a(aO[16],c,k),h=[0,a(aO[16],c,j),w];var
t=iN(z,h);if(t)return t[1];var
A=b(d[40],f);hd(n[1],e[28],Bi,cG,p,A,cG,r);var
B=b(m[3],o)[1];F(n[1],e[28],Bj,ma,B);return b(e[3],Bk)}return h(aY[14],f,c,Bh)}function
Bt(g,k,j,d,c){function
e(c,f){var
l=b(k,c);try{var
r=function(a){return a[1][1]!==g?1:0},d=a(m[22][27],r,l)[1],i=d[1],s=e(Bc(i,d[2],d[3],d[4],c),[0,[0,i,c],f]);return s}catch(d){d=q(d);if(d===K){var
n=b(j,c);try{var
o=function(a){return a[1]!==g?1:0},h=a(m[22][27],o,n)[1],p=e(A$(h,c),[0,[0,h,c],f]);return p}catch(a){a=q(a);if(a===K)return[0,[0,c,f]];throw a}}throw d}}return e(d,c)}function
mg(d,c,b,a){try{var
e=Bt(d,c,b,A5(a),0);return e}catch(a){a=q(a);if(a[1]===ea)return[1,a[2]];throw a}}function
mh(c){var
e=c[2],f=[0,0,me(c)];function
g(y,x){var
f=x[2],e=0,i=0,j=0,g=0,F=x[1];for(;;){if(f){var
k=f[2],o=f[1],c=o[2],p=o[1],q=a4(p);if(q){var
n=q[1],r=n[3],z=n[2];if(y===n[1]){var
l=function(c){return function(b,d){return d?[0,a(m[4],c[4],c[3]),b]:b}}(c),s=c[1],t=s[2],u=s[1];if(1===b(d[25],z)){var
A=l(g,t),f=k,e=[0,[0,r,c],e],i=l(i,u),g=A;continue}var
B=l(g,u),f=k,e=[0,[0,r,c],e],i=l(i,t),g=B;continue}var
C=a(m[4],c[4],c[3]),f=k,e=[0,[0,p,c],e],j=a(m[4],C,j);continue}var
D=a(m[4],c[4],c[3]),f=k,e=[0,[0,P,c],e],j=a(m[4],D,j);continue}var
v=b(m[22][1],i),E=h(m[22][15],m[4],0,i),w=b(m[22][1],g);return[0,[0,[0,y,j+w*E+v*h(m[22][15],m[4],0,g)-w*v],F],e]}}var
i=h(d$[15],g,e,f)[1];function
j(c,b){return a(m[2],c[2],b[2])}return a(m[22][39],j,i)}function
mi(b){var
c=b[1];if(c){var
e=b[2];if(e)return a(d[26],c[1],e[1])}return 0}function
mj(b,g){var
a=g;for(;;){var
c=a4(a);if(c){var
d=c[1],e=d[3],f=d[1];if(f===b)return[0,1,e];if(f<b){var
a=e;continue}return[0,0,a]}return[0,0,P]}}function
mk(I){var
l=me(I),J=0;function
K(c,e){var
b=e[2],f=b[1],g=f[1],j=e[1];if(g){var
h=f[2];if(h){var
i=g[1];if(a(d[26],i,h[1])){var
k=a(m[4],b[4],b[3]);return[0,[0,j,i,b[2],k],c]}return c}}return c}var
n=h(m[22][15],K,J,l),b=n;for(;;){if(b){var
o=b[2],c=b[1],p=c[1],x=c[4],y=c[3],z=c[2],q=a4(p);if(!q){var
b=o;continue}var
r=q[1],A=r[1];if(!b7(r[3])){var
b=o;continue}var
k=[0,[0,A,p,z,y,x]]}else
var
k=0;if(k)var
g=[0,k[1]];else{var
e=n;a:for(;;){if(e){var
f=e[1],w=f[1],s=w,E=e[2],F=f[4],G=f[3],H=f[2];for(;;){var
t=a4(s);if(t){var
u=t[1],v=u[1],D=u[3],B=0,C=function(e){return function(b,c){var
d=c[2];return mj(e,c[1])[1]?mi(d[1])?a(m[4],b,1):b:b}}(v);if(2!==h(m[22][15],C,B,l)){var
s=D;continue}var
j=[0,v]}else
var
j=0;if(!j){var
e=E;continue a}var
g=[0,[0,j[1],w,H,G,F]];break}}else
var
g=0;break}}if(g){var
i=g[1];return[0,[0,[0,i[1],i[2],i[3],i[4]],0],0]}var
L=0,M=function(y,f){var
t=f[1],p=t,o=l,j=y,z=f[4],A=f[3],B=f[2];b:for(;;){var
q=a4(p);if(q){var
r=q[1],s=r[1],w=r[3],d=o,c=0,b=0,x=a(m[5],z,1);for(;;){if(d){var
g=d[2],k=d[1],e=k[2],u=k[1],h=a(m[4],e[3],e[4]),n=mj(s,u),i=n[2];if(0===n[1]){var
d=g,c=a(m[4],c,h),b=[0,[0,i,e],b];continue}if(mi(e[1])){var
d=g,c=a(m[4],c,h),b=[0,[0,i,e],b];continue}var
v=a(m[4],c,h),d=g,c=a(m[4],v,x),b=[0,[0,i,e],b];continue}var
p=w,o=b,j=[0,[0,[0,s,t,B,A],c],j];continue b}}return j}},N=h(m[22][15],M,L,n),O=function(b,a){return dt(b[2],a[2])};return a(m[22][39],O,N)}}function
Bu(g,d){var
i=0;function
j(c,b){var
d=kP(b[1]);return a(e[6],c,d)}var
c=h(m[22][15],j,i,d),f=mg(c,mk,mh,[0,[0,H(c,Bw,g),0,Bv],d]);if(0===f[0]){var
k=f[1][1];try{var
o=[0,mf(I[1],c,k[1])];return o}catch(c){c=q(c);if(b(fR[13],c)){var
l=b(eT[1],c);a(n[2],Bx,l);return 0}throw c}}return 0}function
By(w){var
j=mg(e[8],mk,mh,w);if(0===j[0]){var
i=j[1][2],g=I[1];for(;;){if(i){var
r=i[1],s=r[1],x=i[2],k=mf(g,s,r[2][1]),m=k[1];if(m){var
n=k[2],c=m[1];if(n){var
o=n[1];if(a(d[29],c,Bl))if(a(d[29],Bm,o))var
f=Bn,l=1;else
var
l=0;else
var
l=0;if(!l)var
t=b(d[22],o),u=b(d[24],c),f=a(d[29],u,t)?b(d[24],c):c}else
var
f=a(d[29],c,Bo)?Bp:b(d[24],c)}else{var
p=k[2];if(p)var
q=p[1],v=b(d[22],q),f=a(d[29],Bq,v)?Br:b(d[22],q);else
var
f=Bs}var
i=x,g=h(I[4],s,f,g);continue}var
y=function(c,b,a){return H(c,b,a)};return[0,h(I[12],y,g,P)]}}return[1,j[1]]}function
cS(b,a){return iR(b,a)[1]}function
fU(b,a){if(0===b)if(0===a)return 0;return 1}function
ml(r,q,p){var
j=p[2],k=p[1],l=q[2],m=q[1],n=j[3],f=j[2],g=j[1],o=l[3],h=l[2],i=l[1],c=au(r,i),e=au(r,g),U=0===c[0]?0===c[1]?1:0:0;if(!U){var
V=0===e[0]?0===e[1]?1:0:0;if(!V){var
s=b(d[25],e);if(-1===hc(b(d[25],c),s)){var
t=b(d[15],e),u=a(d[9],n,t),v=b(d[15],c),w=a(d[9],o,v),x=a(d[1],w,u),y=fU(h,f),z=[0,g,b(d[15],e)],A=[0,cS([0,i,b(d[15],c)],z),y,x],B=[0,k,b(d[15],e)];return[0,[0,cS([0,m,b(d[15],c)],B),A]]}if(0===h){var
C=a(d[9],n,Bz),D=a(d[9],c,e),E=b(d[3],D),F=a(d[9],o,E),G=a(d[1],F,C),H=fU(h,f),I=a(d[9],c,e),J=[0,cS([0,i,b(d[3],I)],[0,g,BA]),H,G],K=a(d[9],c,e);return[0,[0,cS([0,m,b(d[3],K)],[0,k,BB]),J]]}if(0===f){var
L=a(d[9],o,BC),M=a(d[9],e,c),N=b(d[3],M),O=a(d[9],n,N),P=a(d[1],O,L),Q=fU(h,f),R=a(d[9],e,c),S=[0,cS([0,g,b(d[3],R)],[0,i,BD]),Q,P],T=a(d[9],e,c);return[0,[0,cS([0,k,b(d[3],T)],[0,m,BE]),S]]}return 0}}return 0}var
mm=[0,function(y,c){function
f(c){switch(c[0]){case
0:var
k=c[1],z=a(m[22][7],y,k);return[0,[0,H(k,BJ,P),z],0];case
1:var
A=c[3],B=c[1],C=f(c[2]),D=f(A),v=0,w=function(a,c){function
b(a,d){var
b=ml(B,c,d);return b?[0,b[1],a]:a}return h(m[22][15],b,a,D)};return h(m[22][15],w,v,C);default:var
E=c[2],F=f(c[1]),G=f(E),I=a(m[23],F,G),x=function(b,d){var
c=d[2],e=d[1];if(0===b[0]){var
f=b[1],a=md(c,0);return typeof
a==="number"?0===a?[1,[0,e,c]]:[0,f]:[0,[0,[0,e,c,a[1],a[2]],f]]}return b},i=h(m[22][15],x,BF,I);if(0===i[0]){var
J=i[1],K=function(k,j){if(0===k[0]){var
t=k[1],l=j[2],m=j[1],n=j[4][1],x=t[2],y=t[1],u=n[2],v=n[1],o=function(e,c,b){if(c){var
f=c[1][3];if(b){var
d=b[1];return a(e,f,d)?[0,[0,m,l,d]]:c}return c}return b?[0,[0,m,l,b[1]]]:0},c=o(d[29],y,v),f=o(d[30],x,u);if(c)if(f){var
g=f[1],h=g[2],p=g[1],i=c[1],q=i[1],w=i[2];if(a(d[29],i[3],g[3]))return[0,[0,c,f]];var
r=a4(h[1]);if(r){var
s=ml(r[1][1],[0,q,w],[0,p,h]);return s?[1,s[1]]:b(e[3],BG)}return[1,[0,cS([0,q,BI],[0,p,BH]),h]]}return[0,[0,c,f]]}return k},j=h(m[22][15],K,BK,J);if(0===j[0]){var
l=j[1],g=l[2],n=l[1];if(n){var
o=n[1],p=o[2],q=o[1];if(g){var
r=g[1];return[0,[0,q,p],[0,[0,r[1],r[2]],0]]}var
t=p,s=q}else{if(!g)return 0;var
u=g[1],t=u[2],s=u[1]}return[0,[0,s,t],0]}return[0,j[1],0]}return[0,i[1],0]}}return f(c)},fU],fV=[0,By,Bu];aF(1027,[0,[0,I[1],I[2],I[3],I[4],I[5],I[6],I[7],I[8],I[9],I[10],I[11],I[12],I[13],I[14],I[15],I[16],I[17],I[18],I[19],I[20],I[21],I[22],I[23],I[24],I[25],I[26]],fV,eb,mm,fT],"Micromega_plugin__Mfourier");var
b_=[0,1],BL=0,BM=p[8],BO=0;function
BP(a){return[1,b(aA[1],a)]}var
fW=[0,A[2],BP,BO,BN,BM,aT],BS=aA[3],iS=[0,function(a){return[0,b(A[2],a),0]},BS,BR,BQ,a3,ay];function
mn(a,i){var
b=i;for(;;){var
d=a[6],e=a[5],f=a[4],g=a[3],h=function(b,c,d,e){return function(a){return ke(e,d,c,b,a)}}(d,e,f,g),c=function(c){function
b(a){if(typeof
a!=="number")switch(a[0]){case
3:var
d=a[1],e=b(a[2]);return c([3,b(d),e]);case
4:var
f=a[1],g=b(a[2]);return c([4,b(f),g])}return c(a)}return b}(h)(b);if(ab(c,b))return c;var
b=c;continue}}function
fX(a){var
e=a[2],c=br(a[1]),f=c[2];return[0,f,e,b(d[3],c[1])]}function
BV(c){var
p=t[1];function
q(d,c){var
b=h2(c[1]);return a(t[7],d,b)}var
r=h(g[20],q,p,c),s=0;function
u(k,j){var
a=0;function
d(b,a){return[0,au(k,a[1]),b]}var
e=h(g[20],d,a,c),f=[1,o[1]],i=b(g[9],e);return[0,[0,cH([0,[1,o[1]],[0,[1,o[1]],i]]),0,f],j]}var
v=h(t[15],u,r,s),i=0;function
j(c,a){return[0,b(d[3],a[3]),c]}var
k=h(g[20],j,i,c),l=[1,o[1]],m=b(g[9],k),n=[0,cH([0,[1,o[1]],[0,[1,o[2]],m]]),0,l],w=[1,o[2]],x=1;function
y(a){return lM(a)?[1,o[2]]:[1,o[1]]}var
z=a(g[17],y,c),A=[0,cH([0,[1,o[1]],[0,[1,o[2]],z]]),x,w],B=[0,n,v];function
f(e,d){var
b=e,a=d;for(;;){if(a){var
c=a[2];if(0===a[1][2]){var
b=b+1|0,a=c;continue}var
g=f(b+1|0,c),h=1;return[0,[0,h0(b+1|0,function(a){return BU},P),h,BT],g]}return 0}}var
C=[0,A,f(1,c)],D=a(e[26],C,B),E=[1,o[1]];return[0,[0,cH([0,[1,o[1]],[0,[1,o[2]],0]]),1,E],D]}function
iT(c){if(b(m[3],b_))return l5(c);var
d=b(fV[1],c);if(0===d[0])return 0;var
e=a(mm[1],c,d[1]);return[0,db(b(m[22][5],e)[1])]}function
BW(a){if(b(m[3],b_))return l6(a);var
c=b(fV[1],a);return 0===c[0]?[0,c[1]]:0}function
BZ(g){try{var
a=iT(g);return a}catch(a){a=q(a);if(a===ft){var
h=BV(g);try{var
c=BW(h);if(c)var
d=c[1],i=a4(d)?[0,db(kQ(2,H(1,BX,d)))]:b(e[3],BY),f=i;else
var
f=0;return f}catch(a){a=q(a);if(b(fR[13],a))return 0;throw a}}throw a}}function
iU(b){var
c=[0,0,s[1]];function
d(b,d){var
c=b[1],e=h(s[4],c,d,b[2]);return[0,a(m[4],c,1),e]}return h(m[22][15],d,c,b)[2]}function
iV(e){var
c=b(m[22][of],e),f=c[2],d=BZ(c[1]);if(d){var
g=d[1],h=iU(f);return[0,a(S[12],h,g)]}return 0}var
ec=[bc,B1,bb(0)];function
iW(k){var
f=k[2],g=k[1],h=g[3],i=g[2],j=g[1];if(a4(j)){var
l=h6(j),c=[1,l];if(a(d[32],c,B2))return[2,g,f];var
m=a(d[12],h,c),n=b(d[25],m);if(a(x[2],n,0)){if(1<=b(d[25],c)){var
o=a(d[9],h,c);return[2,[0,c$(c,j),i,o],[6,l,f]]}throw[0,aV,B3]}switch(i){case
0:return[0,[9,f]];case
1:var
p=a(d[9],h,c),q=b(d[24],p);return[1,[0,c$(c,j),i,q],[9,f]];default:return b(e[3],B4)}}return a(fu(i),B5,h)?0:[0,f]}function
mo(g,f,a){var
c=0;function
d(c,d){var
e=b(f,d);if(e){var
a=b(g,e[1]);if(typeof
a==="number")return c;else
switch(a[0]){case
0:throw[0,ec,a[1]];case
1:return[0,[0,a[1],a[2]],c];default:return[0,[0,a[1],a[2]],c]}}return[0,d,c]}return h(m[22][15],d,c,a)}function
mp(c){return eU(function(f){var
e=bM(function(l){var
c=l[1],g=c[2],h=c[1];function
i(b){var
c=a(d[26],b,B6);return c?c:a(d[26],b,B7)}if(0===g){var
j=a(T[13],i,h),k=function(e){function
c(d){var
c=b(T[9],d[1][1]);return c?c:a(T[10],e,d[1][1])}return a(m[22][21],c,f)},e=a(m[22][61],k,j);return e?[0,e[1]]:0}return 0},f),g=e[1],j=e[2];if(g){var
i=g[1];return eS(h(ao[12],c,i[2],i[1]),j)}return 0},c)}function
iX(b){return a(ao[15],0,b)}function
mq(c,b){if(1===c)return b;var
d=mq(a(m[5],c,1),b);return a(ao[8],b,d)}function
B8(d,c){if(!b(fN[8],c))if(!b(fN[4],c))try{var
e=b(ao[7],B9),f=function(e,c,b){var
f=mq(c,a(s[27],e,d));return a(ao[8],f,b)},g=[0,h(fN[1],f,c,e)];return g}catch(a){a=q(a);if(a===K)return 0;throw a}return 0}function
ed(g,f,d){b(T[1][1],0);var
c=b(m[22][1],d),h=a(e[6],g,hc(c,g));iv[1]=a(e[6],c,h);function
i(e){var
g=e[1];switch(e[2]){case
0:var
c=0;break;case
1:throw[0,aV,B0];case
2:var
c=2;break;default:var
c=1}function
d(c){switch(c[0]){case
0:var
g=b(f[2],c[1]);return b(by[1],g);case
1:var
h=b(aA[4],c[1]);return b(by[2],h);case
2:var
i=c[1],j=d(c[2]),k=d(i);return a(by[3],k,j);case
3:var
l=c[1],m=d(c[2]),n=b(by[5],m),o=d(l);return a(by[3],o,n);case
4:var
p=c[2],q=d(c[1]),r=d(p);return a(by[4],q,r);case
5:var
s=d(c[1]);return b(by[5],s);default:var
t=c[2],u=d(c[1]),v=b(aA[5],t),e=function(c){if(a(x[2],c,0)){var
d=b(f[2],f[4]);return b(by[1],d)}var
g=e(c-1|0);return a(by[4],u,g)};return e(v)}}return[0,d(g),c]}var
j=a(m[22][68],i,d);function
k(c,a){var
d=a[2];return[0,[0,b(T[2],a[1]),d],[1,c]]}return a(m[22][13],k,j)}function
mr(c){function
f(a){return b(T[9],a[1][1])}if(a(m[22][21],f,c))return c;var
g=dj[1];function
i(c,a){var
d=b(T[16],a[1][1]);function
e(c,a,b){return[0,a]}return h(dj[38],e,c,d)}var
j=h(m[22][15],i,g,c);function
k(d,c,a){var
e=b(T[5],d);return[0,[0,[0,b(T[5],c),1],[4,e]],a]}var
d=h(dj[12],k,j,c),l=t[1];function
n(d,c){var
e=b(T[7],c[1][1]);return a(t[7],d,e)}var
o=h(m[22][15],n,l,d);function
p(e,d){var
c=b(T[3],e);return[0,[0,[0,a(T[14],c,c),1],[4,c]],d]}var
e=h(t[15],p,o,d),q=kw(ao[8],e),r=a(m[23],e,q),s=b(ao[2],B_);return a(m[22][68],s,r)}function
ms(i,g){var
c=mp(ed(i,iS,g)),j=iX(c),k=mr(c),l=a(m[23],k,j);function
n(a){var
b=a[1],c=a[2];return[0,fX([0,b[1],b[2]]),c]}var
d=a(m[22][68],n,l),o=0;function
p(d,c){var
f=b(S[2],c[2]);return a(e[6],d,f)}var
q=h(m[22][15],p,o,d),r=a(fY[53],0,q),f=iV(d);return f?[0,F(S[11],hS,A[5],r,f[1])]:0}function
iY(e,d){var
f=ed(e,iS,d);function
g(a){var
b=a[2];return[0,fX(a[1]),b]}var
b=a(m[22][68],g,f),c=iV(b);if(c){var
h=c[1],i=function(a,b){return a},j=a(m[22][13],i,b);return[0,F(S[11],hS,A[5],j,h)]}return 0}function
cT(c){if(typeof
c==="number")return[0,o[2],0];else
switch(c[0]){case
0:var
f=c[1],z=[0,[1,ak(f)]];return[0,bh(f),z];case
1:return[0,o[2],[1,c[1]]];case
2:var
g=cT(c[1]);return[0,g[1],[2,g[2]]];case
3:var
h=c[1],A=h[2],i=cT(h[1]),j=i[2],k=i[1],l=cT(A),m=l[2],n=l[1],d=a(o[17],k,n),p=a(o[15],k,d),q=a(o[15],n,d),B=a(o[10],p,q),r=a(o[10],d,B),C=a(o[23],r,o[2]);return a(x[2],C,0)?[0,o[2],[3,[0,j,m]]]:[0,r,[3,[0,[5,[0,[0,[1,q]],j]],[5,[0,[0,[1,p]],m]]]]];case
4:return b(e[3],B$);case
5:var
s=c[1],D=s[2],t=cT(s[1]),E=t[2],F=t[1],u=cT(D),G=[5,[0,E,u[2]]];return[0,a(o[10],F,u[1]),G];default:var
v=c[1],w=v[2],y=cT(v[1]),H=[6,[0,y[2],w]];return[0,a(o[19],y[1],w),H]}}function
mt(b){var
a=cT(b);return[0,a[1],a[2]]}function
dr(b){switch(b[0]){case
0:return[0,o[2],[0,b[1]]];case
1:return[0,o[2],[1,b[1]]];case
2:return[0,o[2],[2,b[1]]];case
3:var
d=b[1],t=[3,[1,ak(d)]];return[0,bh(d),t];case
4:var
e=b[1],u=[4,[1,ak(e)]];return[0,bh(e),u];case
5:var
f=b[1],v=[5,[1,ak(f)]];return[0,bh(f),v];case
6:var
g=mt(b[1]),h=g[1],w=[6,g[2]];return[0,a(o[10],h,h),w];case
7:return[0,o[2],[7,b[1]]];case
8:var
x=b[2],i=mt(b[1]),y=i[2],z=i[1],j=dr(x),A=[8,y,j[2]];return[0,a(o[10],z,j[1]),A];case
9:var
B=b[2],k=dr(b[1]),l=k[1],C=k[2],m=dr(B),n=m[1],D=m[2],c=a(o[17],l,n),p=a(o[15],l,c),q=a(o[15],n,c),E=a(o[10],p,q);return[0,a(o[10],c,E),[9,[10,[4,[1,q]],C],[10,[4,[1,p]],D]]];default:var
F=b[2],r=dr(b[1]),G=r[2],H=r[1],s=dr(F),I=[10,G,s[2]];return[0,a(o[10],H,s[1]),I]}}function
bS(c){if(typeof
c==="number")return[0,b(A[5],Ca)];else
switch(c[0]){case
0:return[0,b(A[5],c[1])];case
1:var
d=c[1],j=a(m[5],cX(d),1),k=ny(h(m[20][4],d,1,j));return[1,b(A[6],k)];case
2:return[5,bS(c[1])];case
3:var
e=c[1],l=e[1],n=bS(e[2]);return[2,bS(l),n];case
4:var
f=c[1],o=f[1],p=bS(f[2]);return[3,bS(o),p];case
5:var
g=c[1],q=g[1],r=bS(g[2]);return[4,bS(q),r];default:var
i=c[1],s=i[1],t=b(A[3],i[2]);return[6,bS(s),t]}}function
mu(a){var
c=bS(a),d=b(A[5],Cb);return ad(b(A[5],Cc),d,aU,a3,b6,bL,ay,c)}function
iZ(a){if(a){var
c=a[2],d=a[1];if(c){var
e=iZ(c);return[3,[0,b(A[4],d)],e]}return[0,b(A[4],d)]}return 0}function
mv(c){function
e(c){switch(c[0]){case
0:return[0,b(A[4],c[1])];case
1:return[0,b(A[4],c[1])];case
2:return[0,b(A[4],c[1])];case
6:return[1,mu(c[1])];case
7:return iZ(c[1]);case
8:var
h=c[1],i=e(c[2]);return[2,mu(h),i];case
9:var
j=c[1],k=e(c[2]);return[4,e(j),k];case
10:var
l=c[1],m=e(c[2]);return[3,e(l),m];default:var
f=c[1],g=a(d[37],f,Cd);return a(x[2],g,0)?0:[5,b(A[5],f)]}}return mn(iS,e(c))}function
bT(c){if(typeof
c==="number")return Ce;else
switch(c[0]){case
0:var
k=b(d[52],c[1]);return[0,b(A[2],k)];case
1:var
e=c[1],l=a(m[5],cX(e),1),n=ny(h(m[20][4],e,1,l));return[1,b(A[6],n)];case
2:return[5,bT(c[1])];case
3:var
f=c[1],o=f[1],p=bT(f[2]);return[2,bT(o),p];case
4:var
g=c[1],q=g[1],r=bT(g[2]);return[3,bT(q),r];case
5:var
i=c[1],s=i[1],t=bT(i[2]);return[4,bT(s),t];default:var
j=c[1],u=j[1],v=b(A[3],j[2]);return[6,bT(u),v]}}function
mw(a){var
c=bT(a),d=p[6],e=p[7],f=p[8],g=p[5],h=b(A[7],1);return ad(b(A[7],0),h,g,f,e,d,aT,c)}function
mx(c){var
f=dr(c)[2];function
e(k){var
c=k;for(;;)switch(c[0]){case
0:return[0,b(A[4],c[1])];case
1:return[0,b(A[4],c[1])];case
2:return[0,b(A[4],c[1])];case
6:return[1,mw(c[1])];case
7:return iZ(c[1]);case
8:var
i=c[2],f=c[1];if(typeof
f==="number")var
g=0;else
if(0===f[0])var
j=a(d[26],f[1],Cg),g=1;else
var
g=0;if(!g)var
j=0;if(j){var
c=i;continue}var
n=e(i);return[2,mw(f),n];case
9:var
o=c[1],p=e(c[2]);return[4,e(o),p];case
10:var
q=c[1],r=e(c[2]);return[3,e(q),r];default:var
h=c[1],l=a(d[37],h,Cf);if(a(x[2],l,0))return 0;var
m=b(d[52],h);return[5,b(A[2],m)]}}return mn(fW,e(f))}function
Ch(a){var
b=0;function
c(b,c){var
a=iW([0,c[1],c[2]]);if(typeof
a==="number")return b;else
switch(a[0]){case
0:throw[0,ec,a[1]];case
1:return[0,[0,a[1],a[2]],b];default:return[0,[0,a[1],a[2]],b]}}return h(m[22][15],c,b,a)}function
i0(g,c){var
h=b(o[22],c);if(a(x[2],h,0))return[0,o[2],o[1]];var
d=a(o[14],g,c),i=d[1],e=i0(c,d[2]),f=e[2],j=e[1],k=a(o[10],i,f);return[0,f,a(o[8],j,k)]}function
my(n,m,c){return mo(iW,function(o){var
f=o[1],g=m[1],i=f[2],j=f[1],k=g[2],l=g[1],p=o[2],q=m[2],r=f[3],s=g[3];function
h(c,b){var
e=a(S[8],b,p),f=a(S[8],c,q),g=a(S[7],f,e),h=a(d[6],r,b),m=a(d[6],s,c),n=a(d[1],m,h),o=fv(k,i),t=at(b,j);return[0,[0,aK(at(c,l),t),o,n],g]}var
c=au(n,l),e=au(n,j),D=0===c[0]?0===c[1]?1:0:0;if(!D){var
E=0===e[0]?0===e[1]?1:0:0;if(!E){var
t=b(d[25],e),u=hc(b(d[25],c),t);if(a(x[2],u,-1)){var
v=b(d[15],e);return[0,h(v,b(d[15],c))]}if(0===k){var
w=[0,b(d[25],c)],y=a(d[6],e,w),z=b(d[3],y);return[0,h(z,b(d[15],c))]}if(0===i){var
A=b(d[15],e),B=[0,b(d[25],e)],C=a(d[6],c,B);return[0,h(A,b(d[3],C))]}return 0}}return 0},c)}function
Ci(A){var
c=0,b=A;for(;;){if(b){var
l=b[2],e=b[1],n=bM(function(g){return function(f){var
b=f[1],c=g[1];if(0===c[2])if(0===b[2]){var
d=b[1],e=c[1];return kR(function(c,b){var
d=o[2],e=ak(b),f=ak(c),g=a(o[17],f,e),h=a(o[23],g,d);return a(x[2],h,0)},e,d)}return 0}}(e),l),p=n[1];if(!p){var
c=[0,e,c],b=l;continue}var
q=p[1],y=q[2],z=q[1],f=[0,[0,[0,z,e,y]],a(m[22][10],c,n[2])]}else
var
f=[0,0,c];var
r=f[1],B=f[2];if(r){var
g=r[1],s=g[3],t=s[1],u=g[2],v=u[2],h=u[1],i=g[1],C=s[2],D=i[2],E=i[1],F=ak(i[3]),w=i0(ak(D),F),j=[1,w[1]],k=[1,w[2]],G=a(d[6],k,t[3]),H=a(d[6],j,h[3]),I=a(d[1],H,G),J=at(k,t[1]),K=[0,aK(at(j,h[1]),J),0,I],L=a(S[8],k,C),M=a(S[8],j,v);return[0,my(E,[0,K,a(S[7],M,L)],[0,[0,h,v],B])]}return 0}}function
Cj(f){var
b=bM(function(c){var
b=c[1];if(0===b[2]){var
e=b[1];return h5(function(c,b){if(!a(d[26],b,Ck))if(!a(d[26],b,Cl))return 0;return[0,c]},e)}return 0},f),c=b[1],g=b[2];if(c){var
e=c[1];return[0,my(e[1],e[2],g)]}return 0}function
Cm(m){var
c=bM(function(k){var
i=k[1];if(0===i[2]){var
c=i[1];for(;;){var
d=a4(c);if(d){var
b=d[1],e=b[3],j=b[1],f=ak(b[2]),g=h5(function(g){return function(d,c){var
b=ak(c),e=o[2],f=a(o[17],g,b);return a(o[24],f,e)?[0,[0,d,b]]:0}}(f),e);if(g){var
h=g[1];return[0,[0,[0,j,f],[0,h[1],h[2]]]]}var
c=e;continue}return 0}}return 0},m),e=c[1],n=c[2];if(e){var
f=e[1],g=f[2],h=g[1],i=f[1],j=i[2],k=i[1],p=g[2],q=j[1],r=k[1],l=i0(k[2],j[2]),s=[1,l[2]],t=[1,l[1]];return[0,mo(iW,function(g){var
c=g[1],e=c[1],i=g[2],j=c[3],k=c[2],l=au(r,e),m=au(q,e),n=a(d[6],m,s),o=a(d[6],l,t),u=a(d[1],o,n),f=b(d[3],u),v=a(S[8],f,p),w=a(S[7],v,i),x=a(d[6],f,h[3]),y=a(d[1],x,j);return[0,[0,[0,aK(at(f,h[1]),e),k,y],w]]},n)]}return 0}function
i1(a){var
b=[0,Cj,[0,Ci,[0,Cm,0]]];return eU(function(a){return kD(b,a)},a)}function
mz(c){function
e(a){var
c=a[1][1];return e5(function(c,a){return 0!==b(d[25],a)?1:0},c)}return a(m[22][21],e,c)}function
Cq(k,j,g){function
l(F,n){if(mz(n)){var
G=b(m[22][of],n),H=G[2],f=G[1],J=function(a){return 0===a[2]?1:0},u=a(m[22][30],J,f),v=u[2],w=u[1];if(w)var
K=0,L=function(c,b){function
d(a){return dS(b[1],a[1])}return a(m[22][22],d,w)?c:[0,b[1],c]},x=h(m[22][15],L,K,v);else
var
M=function(a){return a[1]},x=a(m[22][14],M,v);var
N=[0,P,Co],O=function(c,e){var
g=fS(c[2]),l=g?a(d[29],g[1],Cn):0;if(l)return c;var
i=b(m[3],b_)?l7(e,f):a(fV[2],e,f);if(i){var
j=i[1],h=c[2],k=c[1];return mb(j,h)?[0,e,j]:[0,k,h]}return c},y=h(m[22][15],O,N,x),z=y[2],A=z[1],Q=y[1];if(A){var
B=z[2];if(B)var
g=[0,[0,A[1],Q,B[1]]],s=1;else
var
s=0}else
var
s=0;if(!s)var
g=0;if(g){var
i=g[1],j=i[3],k=i[2],l=i[1],R=bh(l),T=o[2],U=ak(l),V=a(o[8],U,T),W=bh(j),X=ak(j),Y=[1,a(o[5],o[2],X)],C=iT([0,[0,at([1,W],k),1,Y],f]),Z=b(d[3],[1,V]),D=iT([0,[0,at(b(d[3],[1,R]),k),1,Z],f]);if(C)if(D)var
_=D[1],$=hZ(C[1]),aa=b(m[22][6],$),ab=hZ(_),c=[0,[0,b(m[22][6],ab),[0,l,k,j],aa]],t=1;else
var
t=0;else
var
t=0;if(!t)var
c=b(e[3],Cp)}else
var
c=0;if(c){var
p=c[1],q=p[2],I=q[2],ac=p[3],ad=q[1],ae=p[1],af=b(d[22],q[3]),r=E(F,I,b(d[24],ad),af,n);if(typeof
r!=="number"&&0===r[0]){var
ag=r[1],ah=cH(ac),ai=iU(H),aj=a(S[12],ai,ah),al=cH(ae),am=iU(H);return[0,[1,F,a(S[12],am,al),I,aj,ag]]}return 0}return 0}throw[0,aV,Cr]}function
E(c,j,b,h,g){if(a(d[28],b,h))return Cs;var
e=i(a(m[4],c,1),[0,[0,[0,j,0,b],[2,c]],g]);if(typeof
e!=="number"&&0===e[0]){var
k=e[1],f=E(c,j,a(d[1],b,Ct),h,g);if(typeof
f!=="number"&&0===f[0])return[0,[0,k,f[1]]];return 0}return 0}function
i(c,a){if(mz(a))try{var
d=b(j,a),e=iV(d),f=e?[0,[0,c,e[1],0]]:k?l(c,d):0;return f}catch(a){a=q(a);if(a[1]===ec)return[0,[0,c,a[2],0]];throw a}throw[0,aV,Cu]}var
n=0;function
p(d,c){var
f=b(S[2],c[2]);return a(e[6],d,f)}var
r=h(m[22][15],p,n,g),f=a(m[4],1,r);try{var
v=i(f,Ch(g)),c=v}catch(a){a=q(a);if(a[1]!==ec)throw a;var
c=[0,[0,f,a[2],0]]}if(typeof
c!=="number"&&0===c[0]){var
s=c[1],t=a(m[5],f,1),u=a(fY[53],0,t);return[0,a(S[10],u,s)]}return 0}function
Cv(k,i,c){function
d(d,c){var
f=0;function
g(d,c){var
f=b(S[2],c[2]);return a(e[6],d,f)}var
i=h(m[22][15],g,f,d),j=a(m[4],1,i),k=a(m[5],j,1),l=a(fY[53],0,k);return[0,a(S[10],l,c)]}try{var
f=b(i,c),g=l_(f),j=g?d(f,g[1]):0;return j}catch(a){a=q(a);if(a[1]===ec)return d(c,[0,0,a[2],0]);throw a}}function
i2(e,d,c,a){return b(m[3],b_)?Cv(e,c,a):Cq(d,c,a)}var
fZ=[0,0];function
mA(i,l,g,f){var
j=i[1],d=h(i[2],l,g,f),k=b(m[3],fZ);if(k){var
o=k[1],p=[0,E.caml_sys_getcwd(0)],q=h(bN[14],p,o,Cw),c=b(e[49],q),r=ed(g,fW,f);a(n[1],c,Cx);var
s=a(m[22][68],b0,r),t=b(T[21],Cy);F(n[1],c,Cz,t,s);var
u=typeof
d==="number"?0:0===d[0]?(h(n[1],c,CB,j),1):0;if(!u)h(n[1],c,CA,j);b(e[52],c);b(e[65],c)}return d}function
CC(t,r,q){var
u=ed(r,fW,q),c=b(ao[13],u),e=kz(function(a){return b(T[8],a[1][1])},c)[1],f=s[1];function
g(b,a){return h(s[4],a[1],a[2],b)}var
i=h(m[22][15],g,f,e),j=s[1];function
k(c,a){var
d=a[1][1];return X(function(c,a,e){var
d=B8(i,b(T[1][4],a));return d?h(s[4],a,d[1],c):c},c,d)}var
l=h(m[22][15],k,j,c),n=0;function
o(c,b,a){return[0,b,a]}var
p=h(s[13],o,l,n),v=iX(c),w=a(m[23],v,c),d=a(m[23],p,w);function
x(a){var
b=a[1],c=a[2];return[0,fX([0,b[1],b[2]]),c]}var
y=a(m[22][68],x,d);return i2(a(m[22][68],b0,d),t,i1,y)}function
mB(b){function
c(a){var
b=a[1],c=a[2];return[0,fX([0,b[1],b[2]]),c]}return a(m[22][68],c,b)}function
CD(d,g,f){var
c=ed(g,fW,f);function
h(a){return b(T[9],a[1][1])}if(a(m[22][21],h,c)){var
i=mB(c);return i2(a(m[22][68],b0,c),d,i1,i)}var
e=mp(c),j=iX(e),k=mB(mr(a(m[23],e,j)));return i2(a(m[22][68],b0,c),d,i1,k)}function
mC(c,b,a){return mA([0,CE,CC],c,b,a)}function
mD(c,b,a){return mA([0,CF,CD],c,b,a)}aF(1029,[0,BL,b_,fZ,mv,mx,mC,mD,iY,ms],"Micromega_plugin__Certificate");var
i3=e[8],mE=[0,i3],i4=[0,1],mF=[0,i3];function
mG(a){return[0,b_[1],i4[1],mF[1]]}function
i5(a){return mE[1]}var
i6=[0,1],i7=[0,1],i8=[0,1],CG=[0,1];function
mH(b,a){function
c(b){var
c=b?b[1]:i3;a[1]=c;return 0}function
d(b){return[0,a[1]]}return[0,0,h(g[21],e[17],b,CH),b,d,c]}function
CI(a){i4[1]=a;return 0}var
CL=[0,0,CK,CJ,function(a){return i4[1]},CI];function
CM(a){b_[1]=a;return 0}var
CP=[0,0,CO,CN,function(a){return b_[1]},CM];function
CQ(a){fZ[1]=a;return 0}var
CT=[0,0,CS,CR,function(a){return fZ[1]},CQ];function
CU(a){i6[1]=a;return 0}var
CX=[0,0,CW,CV,function(a){return i6[1]},CU];function
CY(a){i7[1]=a;return 0}var
C1=[0,0,C0,CZ,function(a){return i7[1]},CY];function
C2(a){i8[1]=a;return 0}var
C5=[0,0,C4,C3,function(a){return i8[1]},C2];a(b$[4],0,CP);a(b$[4],0,CX);a(b$[4],0,C1);a(b$[4],0,C5);a(b$[6],0,CT);var
C7=mH(C6,mE);a(b$[3],0,C7);var
C9=mH(C8,mF);a(b$[3],0,C9);a(b$[4],0,CL);var
C$=a(e[26],ee[20],mI),Da=a(e[26],ee[19],C$),Db=a(e[26],[0,C_,0],Da),Dc=a(e[26],ee[21],Db);function
Z(d,c,a){var
e=h(ee[18],d,c,a),f=b(Dg[15],e);return b(l[9],f)}var
Dh=ee[21];function
aB(a){return Z(Di,Dh,a)}function
v(a){return Z(Dj,Dc,a)}function
bU(a){return Z(Dk,Dd,a)}function
aZ(a){return Z(Dl,De,a)}function
bV(a){return Z(Dm,Df,a)}function
bm(a){return Z(Dn,mI,a)}var
a6=[f,function(a){return aB(Do)}],a7=[f,function(a){return aB(Dp)}],cb=[f,function(a){return aB(Dq)}],f0=[f,function(a){return aB(Dr)}],a8=[f,function(a){return aB(Ds)}],ae=[f,function(a){return aB(Dt)}],f1=[f,function(a){return v(Du)}],f2=[f,function(a){return v(Dv)}],cc=[f,function(a){return v(Dw)}],f3=[f,function(a){return aB(Dx)}],f4=[f,function(a){return aB(Dy)}],cd=[f,function(a){return aB(Dz)}],a9=[f,function(a){return aB(DA)}],f5=[f,function(a){return aB(DB)}],f6=[f,function(a){return aB(DC)}],f7=[f,function(a){return aB(DD)}],f8=[f,function(a){return aB(DE)}],f9=[f,function(a){return bU(DF)}],f_=[f,function(a){return bU(DG)}],f$=[f,function(a){return bU(DH)}],ga=[f,function(a){return bU(DI)}],gb=[f,function(a){return bU(DJ)}],L=[f,function(a){return bU(DK)}],gc=[f,function(a){return bU(DL)}],gd=[f,function(a){return bU(DM)}],ge=[f,function(a){return bU(DN)}],aP=[f,function(a){return v(DO)}],aw=[f,function(a){return v(DP)}],ce=[f,function(a){return v(DQ)}],cf=[f,function(a){return v(DR)}],gf=[f,function(a){return bm(DS)}],gg=[f,function(a){return bm(DT)}],gh=[f,function(a){return bm(DU)}],gi=[f,function(a){return bm(DV)}],gj=[f,function(a){return bm(DW)}],gk=[f,function(a){return bm(DX)}],gl=[f,function(a){return bm(DY)}],gm=[f,function(a){return bm(DZ)}],gn=[f,function(a){return bm(D0)}],go=[f,function(a){return bm(D1)}],cg=[f,function(a){return v(D2)}],ch=[f,function(a){return v(D3)}],ci=[f,function(a){return v(D4)}],gp=[f,function(a){return v(D5)}],gq=[f,function(a){return v(D6)}],gr=[f,function(a){return v(D7)}],gs=[f,function(a){return v(D8)}],gt=[f,function(a){return v(D9)}],D$=[f,function(a){return bV(D_)}],Eb=[f,function(a){return bV(Ea)}],Ed=[f,function(a){return bV(Ec)}],Ef=[f,function(a){return bV(Ee)}],a_=[f,function(a){return aB(Eg)}],ef=[f,function(a){return bV(Eh)}],eg=[f,function(a){return bV(Ei)}],eh=[f,function(a){return bV(Ej)}],ei=[f,function(a){return bV(Ek)}],ej=[f,function(a){return bV(El)}],En=[f,function(a){return v(Em)}],Ep=[f,function(a){return v(Eo)}],Er=[f,function(a){return v(Eq)}],ek=[f,function(a){return v(Es)}],el=[f,function(a){return v(Et)}],em=[f,function(a){return v(Eu)}],en=[f,function(a){return v(Ev)}],eo=[f,function(a){return v(Ew)}],Ey=[f,function(a){return aZ(Ex)}],EA=[f,function(a){return aZ(Ez)}],EC=[f,function(a){return aZ(EB)}],EE=[f,function(a){return aZ(ED)}],bz=[f,function(a){return aZ(EF)}],bA=[f,function(a){return aZ(EG)}],bW=[f,function(a){return aZ(EH)}],bB=[f,function(a){return aZ(EI)}],cj=[f,function(a){return aZ(EJ)}],a0=[f,function(a){return aZ(EK)}],gu=[f,function(a){return aZ(EL)}],ck=[f,function(a){return aZ(EM)}],cl=[f,function(a){return aZ(EN)}],gv=[f,function(a){return v(EO)}],gw=[f,function(a){return v(EP)}],gx=[f,function(a){return v(EQ)}],gy=[f,function(a){return v(ER)}],gz=[f,function(a){return v(ES)}],gA=[f,function(a){return v(ET)}],gB=[f,function(a){return v(EU)}],gC=[f,function(a){return v(EV)}],gD=[f,function(a){return v(EW)}],gE=[f,function(a){return v(EX)}],gF=[f,function(a){return v(EY)}],gG=[f,function(a){return v(EZ)}],gH=[f,function(a){return v(E0)}],gI=[f,function(a){return v(E1)}],gJ=[f,function(a){return v(E2)}],gK=[f,function(a){return v(E3)}],gL=[f,function(a){return v(E4)}],gM=[f,function(a){return v(E5)}],gN=[f,function(a){return v(E6)}],gO=[f,function(a){return v(E7)}],gP=[f,function(a){return v(E8)}],gQ=[f,function(a){return v(E9)}],gR=[f,function(a){return v(E_)}],gS=[f,function(a){return bm(E$)}],Fd=[f,function(a){return Z(Fc,Fb,Fa)}],Fh=[f,function(a){return Z(Fg,Ff,Fe)}],Fl=[f,function(a){return Z(Fk,Fj,Fi)}],Fp=[f,function(a){return Z(Fo,Fn,Fm)}],Ft=[f,function(a){return Z(Fs,Fr,Fq)}],Fx=[f,function(a){return Z(Fw,Fv,Fu)}],FB=[f,function(a){return Z(FA,Fz,Fy)}],FF=[f,function(a){return Z(FE,FD,FC)}],cm=[f,function(a){return Z(FI,FH,FG)}],a$=[f,function(a){return Z(FL,FK,FJ)}],gT=[f,function(a){return Z(FO,FN,FM)}],cn=[f,function(a){return Z(FR,FQ,FP)}],M=[bc,FS,bb(0)];function
i9(c,e){var
b=a(l[3],c,e);switch(b[0]){case
9:var
f=b[2],d=a(l[3],c,b[1]);if(12===d[0])return[0,d[1][1][2],f];throw M;case
12:return[0,b[1][1][2],[0]];default:throw M}}function
i_(a,d){var
b=i9(a,d),c=b[1],e=b[2];if(1===c)return 0;if(2===c)return[0,i_(a,J(e,0)[1])];throw M}function
FT(c,a){var
d=b(aA[6],a);return h(n[1],c,FU,d)}function
ep(a){if(a){var
e=[0,ep(a[1])],c=j(f4),g=k===c?f4[1]:f===c?b(i[2],f4):f4;return b(l[23],[0,g,e])}var
d=j(f3);return k===d?f3[1]:f===d?b(i[2],f3):f3}function
eq(a,e){var
b=i9(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw M;switch(d){case
0:return[0,eq(a,J(c,0)[1])];case
1:return[1,eq(a,J(c,0)[1])];default:return 0}}function
bC(a){if(typeof
a==="number"){var
c=j(f$);return k===c?f$[1]:f===c?b(i[2],f$):f$}else{if(0===a[0]){var
g=[0,bC(a[1])],d=j(gb),h=k===d?gb[1]:f===d?b(i[2],gb):gb;return b(l[23],[0,h,g])}var
m=[0,bC(a[1])],e=j(ga),n=k===e?ga[1]:f===e?b(i[2],ga):ga;return b(l[23],[0,n,m])}}function
gU(c,a){var
d=b(aA[4],a);return h(n[1],c,FV,d)}function
mJ(e,d,c){switch(a(l[3],d,c)[0]){case
10:case
12:var
h=U(mK[2],0,0,e,d,c);try{var
g=j(gS),m=[0,h,c],n=k===g?gS[1]:f===g?b(i[2],gS):gS,o=b(l[23],[0,n,m]);F(FW[23],0,e,d,o);var
p=1;return p}catch(a){a=q(a);if(a===K)return 0;throw a}default:return 0}}function
mL(c,b,e){var
d=a(l[3],b,e);switch(d[0]){case
9:var
g=d[2],f=mJ(c,b,d[1]);if(f){var
h=function(a){return mL(c,b,a)};return a(mM[21],h,g)}return f;case
10:case
12:return mJ(c,b,e);default:return 0}}function
mN(a,e){var
b=i9(a,e),c=b[2],d=b[1]-1|0;if(2<d>>>0)throw M;switch(d){case
0:return 0;case
1:return[0,eq(a,J(c,0)[1])];default:return[1,eq(a,J(c,0)[1])]}}function
a1(a){if(typeof
a==="number"){var
c=j(gc);return k===c?gc[1]:f===c?b(i[2],gc):gc}else{if(0===a[0]){var
g=[0,bC(a[1])],d=j(gd),h=k===d?gd[1]:f===d?b(i[2],gd):gd;return b(l[23],[0,h,g])}var
m=[0,bC(a[1])],e=j(ge),n=k===e?ge[1]:f===e?b(i[2],ge):ge;return b(l[23],[0,n,m])}}function
co(c,a){var
d=b(aA[1],a),e=b(o[33],d);return h(n[1],c,FX,e)}function
cU(a){var
d=bC(a[2]),e=[0,a1(a[1]),d],c=j(ce),g=k===c?ce[1]:f===c?b(i[2],ce):ce;return b(l[23],[0,g,e])}function
FY(c,m){var
d=a(l[3],c,m);if(9===d[0]){var
e=d[2],n=d[1],g=j(ce),o=k===g?ce[1]:f===g?b(i[2],ce):ce;if(h(l[aq],c,n,o)){var
p=eq(c,J(e,1)[2]);return[0,mN(c,J(e,0)[1]),p]}throw M}throw M}function
bD(a){if(typeof
a==="number"){if(0===a){var
d=j(gf);return k===d?gf[1]:f===d?b(i[2],gf):gf}var
e=j(gg);return k===e?gg[1]:f===e?b(i[2],gg):gg}else
switch(a[0]){case
0:var
z=[0,cU(a[1])],g=j(gh),A=k===g?gh[1]:f===g?b(i[2],gh):gh;return b(l[23],[0,A,z]);case
1:var
B=[0,a1(a[1])],h=j(gi),C=k===h?gi[1]:f===h?b(i[2],gi):gi;return b(l[23],[0,C,B]);case
2:var
D=a[1],E=bD(a[2]),F=[0,bD(D),E],m=j(gj),G=k===m?gj[1]:f===m?b(i[2],gj):gj;return b(l[23],[0,G,F]);case
3:var
H=a[1],I=bD(a[2]),J=[0,bD(H),I],n=j(gk),K=k===n?gk[1]:f===n?b(i[2],gk):gk;return b(l[23],[0,K,J]);case
4:var
M=a[1],N=bD(a[2]),O=[0,bD(M),N],o=j(gl),P=k===o?gl[1]:f===o?b(i[2],gl):gl;return b(l[23],[0,P,O]);case
5:var
c=a[2],Q=a[1];if(0===c[0])var
R=a1(c[1]),p=j(cd),S=k===p?cd[1]:f===p?b(i[2],cd):cd,q=j(L),T=k===q?L[1]:f===q?b(i[2],L):L,r=j(f7),U=[0,T,S,R],V=k===r?f7[1]:f===r?b(i[2],f7):f7,s=b(l[23],[0,V,U]);else
var
Y=ep(c[1]),u=j(cd),Z=k===u?cd[1]:f===u?b(i[2],cd):cd,v=j(L),_=k===v?L[1]:f===v?b(i[2],L):L,w=j(f8),$=[0,_,Z,Y],aa=k===w?f8[1]:f===w?b(i[2],f8):f8,s=b(l[23],[0,aa,$]);var
W=[0,bD(Q),s],t=j(gm),X=k===t?gm[1]:f===t?b(i[2],gm):gm;return b(l[23],[0,X,W]);case
6:var
ab=[0,bD(a[1])],x=j(gn),ac=k===x?gn[1]:f===x?b(i[2],gn):gn;return b(l[23],[0,ac,ab]);default:var
ad=[0,bD(a[1])],y=j(go),ae=k===y?go[1]:f===y?b(i[2],go):go;return b(l[23],[0,ae,ad])}}function
i$(c,d,a){if(a){var
h=a[1],m=i$(c,d,a[2]),n=[0,c,b(d,h),m],e=j(f1),o=k===e?f1[1]:f===e?b(i[2],f1):f1;return b(l[23],[0,o,n])}var
g=j(f2),p=[0,c],q=k===g?f2[1]:f===g?b(i[2],f2):f2;return b(l[23],[0,q,p])}function
mO(d,w,a){function
c(a){switch(a[0]){case
0:var
x=[0,d,b(w,a[1])],h=j(gw),y=k===h?gw[1]:f===h?b(i[2],gw):gw;return b(l[23],[0,y,x]);case
1:var
z=[0,d,bC(a[1])],m=j(gv),A=k===m?gv[1]:f===m?b(i[2],gv):gv;return b(l[23],[0,A,z]);case
2:var
B=a[1],C=c(a[2]),D=[0,d,c(B),C],n=j(gx),E=k===n?gx[1]:f===n?b(i[2],gx):gx;return b(l[23],[0,E,D]);case
3:var
F=a[1],G=c(a[2]),H=[0,d,c(F),G],o=j(gA),I=k===o?gA[1]:f===o?b(i[2],gA):gA;return b(l[23],[0,I,H]);case
4:var
J=a[1],K=c(a[2]),L=[0,d,c(J),K],p=j(gz),M=k===p?gz[1]:f===p?b(i[2],gz):gz;return b(l[23],[0,M,L]);case
5:var
N=[0,d,c(a[1])],q=j(gy),O=k===q?gy[1]:f===q?b(i[2],gy):gy;return b(l[23],[0,O,N]);default:var
r=a[2],P=a[1];if(r)var
u=[0,bC(r[1])],e=j(f_),v=k===e?f_[1]:f===e?b(i[2],f_):f_,s=b(l[23],[0,v,u]);else
var
g=j(f9),s=k===g?f9[1]:f===g?b(i[2],f9):f9;var
Q=[0,d,c(P),s],t=j(gB),R=k===t?gB[1]:f===t?b(i[2],gB):gB;return b(l[23],[0,R,Q])}}return c(a)}function
mP(d,m,a){function
c(a){switch(a[0]){case
0:var
n=[0,d,b(m,a[1])],e=j(gD),o=k===e?gD[1]:f===e?b(i[2],gD):gD;return b(l[23],[0,o,n]);case
1:var
p=a[1],q=c(a[2]),r=[0,d,bC(p),q],g=j(gE),s=k===g?gE[1]:f===g?b(i[2],gE):gE;return b(l[23],[0,s,r]);default:var
t=a[2],u=a[1],v=c(a[3]),w=bC(t),x=[0,d,c(u),w,v],h=j(gC),y=k===h?gC[1]:f===h?b(i[2],gC):gC;return b(l[23],[0,y,x])}}return c(a)}function
bE(d,c,a){function
b(c,a){switch(a[0]){case
0:return F(n[1],c,F2,d,a[1]);case
1:return V(n[1],c,F3,gU,a[1],b,a[2]);default:return nz(n[1],c,F4,b,a[1],gU,a[2],b,a[3])}}return b(c,a)}function
ds(a,e,h){var
g=j(a),c=k===g?a[1]:f===g?b(i[2],a):a;function
d(a){if(typeof
a==="number"){var
g=j(gR),r=[0,c],s=k===g?gR[1]:f===g?b(i[2],gR):gR;return b(l[23],[0,s,r])}else
switch(a[0]){case
0:var
t=[0,c,ep(a[1])],h=j(gL),u=k===h?gL[1]:f===h?b(i[2],gL):gL;return b(l[23],[0,u,t]);case
1:var
v=[0,c,mP(c,e,a[1])],m=j(gM),w=k===m?gM[1]:f===m?b(i[2],gM):gM;return b(l[23],[0,w,v]);case
2:var
x=a[1],y=d(a[2]),z=[0,c,mP(c,e,x),y],n=j(gO),A=k===n?gO[1]:f===n?b(i[2],gO):gO;return b(l[23],[0,A,z]);case
3:var
B=a[1],C=d(a[2]),D=[0,c,d(B),C],o=j(gN),E=k===o?gN[1]:f===o?b(i[2],gN):gN;return b(l[23],[0,E,D]);case
4:var
F=a[1],G=d(a[2]),H=[0,c,d(F),G],p=j(gP),I=k===p?gP[1]:f===p?b(i[2],gP):gP;return b(l[23],[0,I,H]);default:var
J=[0,c,b(e,a[1])],q=j(gQ),K=k===q?gQ[1]:f===q?b(i[2],gQ):gQ;return b(l[23],[0,K,J])}}return d(h)}function
bX(e,c,b){function
d(c,b){if(typeof
b==="number")return a(n[1],c,F5);else
switch(b[0]){case
0:return F(n[1],c,F6,FT,b[1]);case
1:var
f=b[1],g=function(a,b){return bE(e,a,b)};return F(n[1],c,F7,g,f);case
2:var
h=b[2],i=b[1],j=function(a,b){return bE(e,a,b)};return V(n[1],c,F8,j,i,d,h);case
3:return V(n[1],c,F9,d,b[1],d,b[2]);case
4:return V(n[1],c,F_,d,b[1],d,b[2]);default:return F(n[1],c,F$,e,b[1])}}return d(c,b)}function
mQ(d,p,c){var
r=c[2],s=c[1],t=mO(d,p,c[3]);switch(r){case
0:var
e=j(gF),a=k===e?gF[1]:f===e?b(i[2],gF):gF;break;case
1:var
g=j(gG),a=k===g?gG[1]:f===g?b(i[2],gG):gG;break;case
2:var
h=j(gH),a=k===h?gH[1]:f===h?b(i[2],gH):gH;break;case
3:var
m=j(gJ),a=k===m?gJ[1]:f===m?b(i[2],gJ):gJ;break;case
4:var
n=j(gI),a=k===n?gI[1]:f===n?b(i[2],gI):gI;break;default:var
o=j(gK),a=k===o?gK[1]:f===o?b(i[2],gK):gK}var
u=[0,d,mO(d,p,s),a,t],q=j(gT),v=k===q?gT[1]:f===q?b(i[2],gT):gT;return b(l[23],[0,v,u])}function
gV(e,d,c){try{var
m=function(g){var
a=g[1],c=j(a),m=k===c?a[1]:f===c?b(i[2],a):a;return h(l[aq],e,d,m)},n=a(g[33],m,c)[2];return n}catch(a){a=q(a);if(a===K)throw M;throw a}}var
mR=[0,[0,D$,5],[0,[0,Eb,3],[0,[0,Ef,4],[0,[0,Ed,2],0]]]],mS=[0,[0,Ey,5],[0,[0,EA,3],[0,[0,EE,4],[0,[0,EC,2],0]]]],mT=[0,[0,Ep,4],[0,[0,En,2],[0,[0,Er,0],0]]];function
mU(a,c,b){return U(Ga[81],0,a[1],a[2],c,b)}function
Gb(d,c){var
a=c[2],e=c[1],g=d[2],m=a.length-1;if(2===m){var
p=a[1],q=a[2];return[0,gV(g,e,mR),p,q]}if(3===m){var
r=a[1],n=j(a_),s=k===n?a_[1]:f===n?b(i[2],a_):a_;if(h(l[aq],g,e,s)){var
o=j(L),t=k===o?L[1]:f===o?b(i[2],L):L;if(mU(d,r,t)){var
u=J(a,2)[3];return[0,0,J(a,1)[2],u]}}throw M}throw M}function
Gc(d,c){var
a=c[2],e=c[1],g=d[2],m=a.length-1;if(2===m){var
p=a[1],q=a[2];return[0,gV(g,e,mS),p,q]}if(3===m){var
r=a[1],s=a[2],t=a[3],n=j(a_),u=k===n?a_[1]:f===n?b(i[2],a_):a_;if(h(l[aq],g,e,u)){var
o=j(aw),v=k===o?aw[1]:f===o?b(i[2],aw):aw;if(mU(d,r,v))return[0,0,s,t]}throw M}throw M}function
Gd(c,b){var
a=b[2],d=b[1];if(2===a.length-1){var
e=J(a,1)[2],f=J(a,0)[1];return[0,gV(c[2],d,mT),f,e]}throw M}function
gW(a){return[0,0,a]}function
mV(c,g,f){var
d=c[2],e=F(l[oA],c[1],d,g,f);if(e){var
i=e[1],j=b(mW[155],d),k=h(Gf[4],0,j,i);try{var
m=a(mW[37],d,k)}catch(a){a=q(a);if(a[1]===Gg[26])return 0;throw a}return[0,[0,c[1],m]]}return 0}function
gX(c,d){function
f(d,a,c,b){if(a){var
g=a[1],i=a[2],h=mV(d,g,b);if(h)return[0,h[1],a,c];var
e=f(d,i,c+1|0,b);return[0,e[1],[0,g,e[2]],e[3]]}return[0,d,[0,b,0],c]}var
a=f(c[2],c[1],1,d),e=a[2],g=a[1];return[0,[0,e,g],b(A[1],a[3])]}function
mX(c,d){var
a=c[1],b=1,e=c[2];for(;;){if(a){var
f=a[2];if(mV(e,a[1],d))return b;var
a=f,b=b+1|0;continue}throw[0,l2,Gh]}}function
ja(o,p,B,A,d,c){function
s(c,b){var
a=gX(c,b);return[0,[1,a[2]],a[1]]}function
e(c,d){function
C(g,f,b){var
h=b[2],c=e(g,b[1]),i=c[1],d=e(c[2],h),j=d[2];return[0,a(f,i,d[1]),j]}try{var
L=[0,[0,a(p,o,d)],c];return L}catch(p){p=q(p);if(p===M){var
r=a(l[3],o[2],d);if(9===r[0]){var
m=r[2],t=r[1];if(10===a(l[3],o[2],t)[0]){var
D=o[2];try{var
y=function(d){var
a=d[1],c=j(a),e=k===c?a[1]:f===c?b(i[2],a):a;return h(l[aq],D,t,e)},z=a(g[33],y,A)[2],n=z}catch(a){a=q(a);if(a!==K)throw a;var
n=Ge}if(typeof
n==="number"){if(0===n){var
u=e(c,J(m,0)[1]);return[0,[5,u[1]],u[2]]}try{var
w=e(c,J(m,0)[1]),E=w[2],F=w[1],G=[0,a(B,F,J(m,1)[2]),E];return G}catch(a){a=q(a);if(a===M){var
v=gX(c,d);return[0,[1,v[2]],v[1]]}throw a}}else{if(0===n[0]){var
H=n[1],I=J(m,1)[2];return C(c,H,[0,J(m,0)[1],I])}var
x=gX(c,d);return[0,[1,x[2]],x[1]]}}return s(c,d)}return s(c,d)}throw p}}return e(d,c)}var
Gi=[0,[0,eh,0],[0,[0,ej,1],0]],Gj=[0,[0,ei,[0,function(b,a){return[4,b,a]}]],Gi],Gk=[0,[0,eg,[0,function(b,a){return[3,b,a]}]],Gj],Gl=[0,[0,ef,[0,function(b,a){return[2,b,a]}]],Gk],Gm=[0,[0,em,0],[0,[0,eo,1],0]],Gn=[0,[0,en,[0,function(b,a){return[4,b,a]}]],Gm],Go=[0,[0,el,[0,function(b,a){return[3,b,a]}]],Gn],Gp=[0,[0,ek,[0,function(b,a){return[2,b,a]}]],Go],Gq=[0,[0,bW,0],[0,[0,a0,1],0]],Gr=[0,[0,bB,[0,function(b,a){return[4,b,a]}]],Gq],Gs=[0,[0,bA,[0,function(b,a){return[3,b,a]}]],Gr],Gt=[0,[0,bz,[0,function(b,a){return[2,b,a]}]],Gs];function
jb(d,c,b){return a(d,c[2],b)}function
mY(e,b,d){try{var
c=a(e,b,d);return c}catch(c){c=q(c);if(c===M){if(mL(b[1],b[2],d))return a(e,b,h(Gu[6],b[1],b[2],d));throw M}throw c}}function
jc(a,b){return jb(mN,a,b)}function
mZ(a,b){return jb(FY,a,b)}function
Gv(a,b){return jb(i_,a,b)}function
m0(c){function
d(g,f){var
h=gW(c),e=bq(a(m0(c),h,f)[1]);if(e){var
d=e[1];if(typeof
d!=="number"&&1===d[0])return Gw;return[6,g,b(p[17],d)]}throw M}return function(a,b){return ja(c,jc,d,Gl,a,b)}}var
Gx=0,Gy=[0,[0,bB,function(b,a){return[4,b,a]}],Gx],Gz=[0,[0,bA,function(b,a){return[3,b,a]}],Gy],GA=[0,[0,bz,function(b,a){return[2,b,a]}],Gz];function
GC(m,d){var
c=m[2];function
e(n){var
o=a(l[3],c,n);switch(o[0]){case
9:var
d=o[2],g=o[1];try{var
C=gV(c,g,GA),D=e(J(d,0)[1]),E=a(C,D,e(J(d,1)[2]));return E}catch(a){a=q(a);if(a===M){var
p=j(cj),x=k===p?cj[1]:f===p?b(i[2],cj):cj;if(h(l[aq],c,g,x)){var
r=e(J(d,0)[1]);if(ay(aJ(r),GB))throw M;return[6,r]}var
s=j(a0),y=k===s?a0[1]:f===s?b(i[2],a0):a0;if(h(l[aq],c,g,y)){var
z=[1,mY(Gv,m,J(d,1)[2])];return[5,e(J(d,0)[1]),z]}var
t=j(cl),A=k===t?cl[1]:f===t?b(i[2],cl):cl;if(h(l[aq],c,g,A))return[0,mZ(m,J(d,0)[1])];var
u=j(ck),B=k===u?ck[1]:f===u?b(i[2],ck):ck;if(h(l[aq],c,g,B))return[1,mY(jc,m,J(d,0)[1])];throw M}throw a}case
10:var
v=j(cg),F=k===v?cg[1]:f===v?b(i[2],cg):cg;if(h(l[aq],c,n,F))return 0;var
w=j(ch),G=k===w?ch[1]:f===w?b(i[2],ch):ch;if(h(l[aq],c,n,G))return 1;throw M;default:throw M}}return e(d)}function
GD(d){function
a(c,e){var
a=jc(d,e);if(typeof
a!=="number"&&1===a[0]){if(0===c[0])return[0,hD(c[1],a)];throw M}return[6,c,b(p[17],a)]}return function(b,c){return ja(d,mZ,a,Gp,b,c)}}function
GE(a){function
c(d,c){var
e=i_(a[2],c);return[6,d,b(jP[1],e)]}return function(b,d){return ja(a,GC,c,Gt,b,d)}}function
jd(n,g,m,k,c){var
d=a(l[3],c[2],k);if(9===d[0]){var
f=a(n,c,[0,d[1],d[2]]),o=f[3],p=f[1],i=h(g,c,m,f[2]),q=i[1],j=h(g,c,i[2],o);return[0,[0,q,p,j[1]],j[2]]}return b(e[3],GF)}function
gY(a,b,c){return jd(Gb,m0,a,b,c)}function
gZ(a,b,c){return jd(Gd,GD,a,b,c)}function
GG(a,b,c){return jd(Gc,GE,a,b,c)}function
GH(b,a){return[2,b,a]}function
GI(b,a){return[3,b,a]}function
GJ(b,a){return[2,[5,b,0,a],[5,a,0,b]]}function
GK(b,a){return[5,b,0,a]}function
g0(e,d,c,b){if(typeof
c!=="number"&&0===c[0])if(typeof
b!=="number"&&0===b[0])return[0,d];return a(e,c,b)}function
je(d,c,a){var
e=F(mK[3],0,d,c,a);return b(GL[10],e)}function
m1(d,o,n,g,e){var
m=d[2];function
B(a){return je(d[1],d[2],a)}function
K(e,c,a){try{var
f=h(o,e,a,d),g=f[2],i=f[1],j=[0,[1,i,[0,c,a]],g,b(c9[2],c)];return j}catch(b){b=q(b);if(b===M){if(B(a))return[0,[0,a],e,c];throw M}throw b}}function
c(g,e,d){var
n=a(l[3],m,d);switch(n[0]){case
6:var
E=n[3],O=n[2],af=0===n[1][1]?0:h(l[121][13],m,1,E)?0:1;if(!af){var
q=c(g,e,O),P=q[1],r=c(q[2],q[3],E),Q=r[3],R=r[2];return[0,g0(GK,d,P,r[1]),R,Q]}break;case
9:var
o=n[2],p=n[1],F=o.length-1;if(!(3<=F))switch(F){case
0:break;case
1:var
S=o[1],G=j(cb),T=k===G?cb[1]:f===G?b(i[2],cb):cb;if(h(l[aq],m,p,T)){var
s=c(g,e,S);return[0,[4,s[1]],s[2],s[3]]}break;default:var
t=o[1],u=o[2],H=j(a6),U=k===H?a6[1]:f===H?b(i[2],a6):a6;if(h(l[aq],m,p,U)){var
v=c(g,e,t),V=v[1],w=c(v[2],v[3],u),W=w[3],X=w[2];return[0,g0(GH,d,V,w[1]),X,W]}var
I=j(a7),Y=k===I?a7[1]:f===I?b(i[2],a7):a7;if(h(l[aq],m,p,Y)){var
x=c(g,e,t),Z=x[1],y=c(x[2],x[3],u),_=y[3],$=y[2];return[0,g0(GI,d,Z,y[1]),$,_]}var
J=j(f0),aa=k===J?f0[1]:f===J?b(i[2],f0):f0;if(h(l[aq],m,p,aa)){var
z=c(g,e,t),ab=z[1],A=c(z[2],z[3],u),ac=A[3],ad=A[2];return[0,g0(GJ,d,ab,A[1]),ad,ac]}}return K(g,e,d)}var
C=j(a8),L=k===C?a8[1]:f===C?b(i[2],a8):a8;if(h(l[aq],m,d,L))return[0,0,g,e];var
D=j(ae),N=k===D?ae[1]:f===D?b(i[2],ae):ae;if(h(l[aq],m,d,N))return[0,1,g,e];if(B(d))return[0,[0,d],g,e];throw M}return c(n,g,e)}function
m2(g,m,a){function
c(a,h){var
c=j(a9),m=k===c?a9[1]:f===c?b(i[2],a9):a9,d=j(a9),n=[0,m,h],o=k===d?a9[1]:f===d?b(i[2],a9):a9,p=b(mM[12],[0,g,[0,l[16],[0,o,n]]]),e=j(a),q=k===e?a[1]:f===e?b(i[2],a):a;return b(l[23],[0,q,p])}function
d(a){if(typeof
a==="number")return 0===a?c(Fd,0):c(Fh,0);else
switch(a[0]){case
0:return c(FB,[0,a[1],0]);case
1:var
n=a[1],e=j(f6),o=0,p=k===e?f6[1]:f===e?b(i[2],f6):f6;return c(Fx,[0,b(m,n),[0,p,o]]);case
2:var
q=a[1],r=[0,d(a[2]),0];return c(Fl,[0,d(q),r]);case
3:var
s=a[1],t=[0,d(a[2]),0];return c(Fp,[0,d(s),t]);case
4:return c(Ft,[0,d(a[1]),0]);default:var
u=a[1],v=[0,d(a[3]),0],g=j(a9),w=k===g?a9[1]:f===g?b(i[2],a9):a9,h=j(f5),x=[0,w],y=k===h?f5[1]:f===h?b(i[2],f5):f5,z=[0,b(l[23],[0,y,x]),v];return c(FF,[0,d(u),z])}}return d(a)}function
jf(b,a){function
d(h,g){var
b=h,a=g;for(;;){if(typeof
a==="number")var
c=0;else
switch(a[0]){case
0:return gX(b,a[1])[1];case
4:var
a=a[1];continue;case
5:var
f=a[3],e=a[1],c=1;break;case
1:var
c=0;break;default:var
f=a[2],e=a[1],c=1}if(c){var
b=d(b,e),a=f;continue}return b}}return d(gW(b),a)}var
g1=[f,function(w){function
n(c){var
a=c[1],e=c[2],d=j(a),g=k===d?a[1]:f===d?b(i[2],a):a;return[0,e,g]}var
o=a(g[17],n,mR);function
p(a){var
c=b(aA[5],a);return a1(b(A[7],c))}var
c=j(ej),q=k===c?ej[1]:f===c?b(i[2],ej):ej,d=j(ei),r=k===d?ei[1]:f===d?b(i[2],ei):ei,e=j(eh),s=k===e?eh[1]:f===e?b(i[2],eh):eh,h=j(eg),t=k===h?eg[1]:f===h?b(i[2],eg):eg,l=j(ef),u=k===l?ef[1]:f===l?b(i[2],ef):ef,m=j(L),v=k===m?L[1]:f===m?b(i[2],L):L;return[0,v,a1,u,t,s,r,q,p,o]}],g2=[f,function(w){function
n(c){var
a=c[1],e=c[2],d=j(a),g=k===d?a[1]:f===d?b(i[2],a):a;return[0,e,g]}var
o=a(g[17],n,mT);function
p(a){var
c=b(aA[5],a);return a1(b(A[7],c))}var
c=j(eo),q=k===c?eo[1]:f===c?b(i[2],eo):eo,d=j(en),r=k===d?en[1]:f===d?b(i[2],en):en,e=j(em),s=k===e?em[1]:f===e?b(i[2],em):em,h=j(el),t=k===h?el[1]:f===h?b(i[2],el):el,l=j(ek),u=k===l?ek[1]:f===l?b(i[2],ek):ek,m=j(aP),v=k===m?aP[1]:f===m?b(i[2],aP):aP;return[0,v,cU,u,t,s,r,q,p,o]}];function
bn(a){if(typeof
a==="number"){if(0===a){var
d=j(cg);return k===d?cg[1]:f===d?b(i[2],cg):cg}var
e=j(ch);return k===e?ch[1]:f===e?b(i[2],ch):ch}else
switch(a[0]){case
0:var
u=[0,cU(a[1])],g=j(cl),v=k===g?cl[1]:f===g?b(i[2],cl):cl;return b(l[23],[0,v,u]);case
1:var
w=[0,a1(a[1])],h=j(ck),x=k===h?ck[1]:f===h?b(i[2],ck):ck;return b(l[23],[0,x,w]);case
2:var
y=a[1],z=bn(a[2]),A=[0,bn(y),z],m=j(bz),B=k===m?bz[1]:f===m?b(i[2],bz):bz;return b(l[23],[0,B,A]);case
3:var
C=a[1],D=bn(a[2]),E=[0,bn(C),D],n=j(bA),F=k===n?bA[1]:f===n?b(i[2],bA):bA;return b(l[23],[0,F,E]);case
4:var
G=a[1],H=bn(a[2]),I=[0,bn(G),H],o=j(bB),J=k===o?bB[1]:f===o?b(i[2],bB):bB;return b(l[23],[0,J,I]);case
5:var
c=a[2],p=a[1];if(0===c[0]){var
K=a1(c[1]),L=[0,bn(p),K],q=j(gu),M=k===q?gu[1]:f===q?b(i[2],gu):gu;return b(l[23],[0,M,L])}var
N=ep(c[1]),O=[0,bn(p),N],r=j(a0),P=k===r?a0[1]:f===r?b(i[2],a0):a0;return b(l[23],[0,P,O]);case
6:var
Q=[0,bn(a[1])],s=j(cj),R=k===s?cj[1]:f===s?b(i[2],cj):cj;return b(l[23],[0,R,Q]);default:var
S=[0,bn(a[1])],t=j(bW),T=k===t?bW[1]:f===t?b(i[2],bW):bW;return b(l[23],[0,T,S])}}var
g3=[f,function(w){function
n(c){var
a=c[1],e=c[2],d=j(a),g=k===d?a[1]:f===d?b(i[2],a):a;return[0,e,g]}var
o=a(g[17],n,mS);function
p(a){var
c=b(aA[5],a);return ep(b(A[4],c))}var
c=j(a0),q=k===c?a0[1]:f===c?b(i[2],a0):a0,d=j(bB),r=k===d?bB[1]:f===d?b(i[2],bB):bB,e=j(bW),s=k===e?bW[1]:f===e?b(i[2],bW):bW,h=j(bA),t=k===h?bA[1]:f===h?b(i[2],bA):bA,l=j(bz),u=k===l?bz[1]:f===l?b(i[2],bz):bz,m=j(aw),v=k===m?aw[1]:f===m?b(i[2],aw):aw;return[0,v,bn,u,t,s,r,q,p,o]}];function
m3(i,h,g){var
c=[0,i,h,g];for(;;){var
e=c[1];if(0===e)return c[3];var
d=c[2];if(d){var
f=d[1],j=c[3],k=d[2],m=f[2],n=[0,a(m4[4],f[1],0),m,j],c=[0,e-1|0,k,b(l[20],n)];continue}throw[0,aV,GM]}}function
m5(y,d,c){function
o(d){var
c=d;for(;;)switch(c[0]){case
0:return t[1];case
1:var
e=b(aA[4],c[1]);return b(t[5],e);case
5:case
6:var
c=c[1];continue;default:var
f=c[1],g=o(c[2]),h=o(f);return a(t[7],h,g)}}function
r(j){var
b=j;for(;;){if(typeof
b==="number")var
c=0;else
switch(b[0]){case
1:var
d=b[1],g=d[1],h=o(d[3]),i=o(g);return a(t[7],i,h);case
4:var
b=b[1];continue;case
5:var
f=b[3],e=b[1],c=1;break;case
0:var
c=0;break;default:var
f=b[2],e=b[1],c=1}if(c){var
k=r(f),l=r(e);return a(t[7],l,k)}return t[1]}}var
x=r(c),z=b(t[21],x);function
A(b,a){return[0,a,b+1|0]}var
s=a(g[18],A,z),u=jf(y,c);function
B(g){var
h=d[1],c=b(e[22],g[2]),f=a(e[17],GN,c);return[0,b(ca[1][6],f),h]}var
p=a(g[17],B,s),C=u[1];function
D(f,h){var
g=l[16],c=b(e[22],f+1|0),d=a(e[17],GO,c);return[0,b(ca[1][6],d),g]}var
v=a(g[18],D,C);function
E(b,a){return[0,a[1],b[1]]}var
F=h(g[23],E,s,p);function
w(f,c){function
e(c){switch(c[0]){case
0:return b(d[2],c[1]);case
1:var
h=b(aA[4],c[1]),i=f+a(g[38],h,s)|0;return b(l[10],i);case
2:var
j=c[1],k=e(c[2]),m=[0,e(j),k];return b(l[23],[0,d[3],m]);case
3:var
n=c[1],o=e(c[2]),p=[0,e(n),o];return b(l[23],[0,d[4],p]);case
4:var
q=c[1],r=e(c[2]),t=[0,e(q),r];return b(l[23],[0,d[6],t]);case
5:var
u=[0,e(c[1])];return b(l[23],[0,d[5],u]);default:var
v=c[1],w=b(d[8],c[2]),x=[0,e(v),w];return b(l[23],[0,d[7],x])}}return e(c)}function
G(m,e,c){try{var
p=[0,a(g[38],m,d[9]),[0,e,c]],r=b(l[23],p);return r}catch(a){a=q(a);if(a===K){var
n=[0,d[1],e,c],h=j(a_),o=k===h?a_[1]:f===h?b(i[2],a_):a_;return b(l[23],[0,o,n])}throw a}}function
m(d,c,a){if(typeof
a==="number"){if(0===a){var
g=j(a8);return k===g?a8[1]:f===g?b(i[2],a8):a8}var
n=j(ae);return k===n?ae[1]:f===n?b(i[2],ae):ae}else
switch(a[0]){case
0:var
v=d+mX(u,a[1])|0;return b(l[10],v);case
1:var
e=a[1],r=e[2],s=e[1],t=w(c,e[3]);return G(r,w(c,s),t);case
2:var
x=a[1],y=m(d,c,a[2]),z=[0,m(d,c,x),y],o=j(a6),A=k===o?a6[1]:f===o?b(i[2],a6):a6;return b(l[23],[0,A,z]);case
3:var
B=a[1],C=m(d,c,a[2]),D=[0,m(d,c,B),C],p=j(a7),E=k===p?a7[1]:f===p?b(i[2],a7):a7;return b(l[23],[0,E,D]);case
4:var
F=a[1],q=j(ae),H=k===q?ae[1]:f===q?b(i[2],ae):ae,I=m(d,c,F);return h(l[35],I,0,H);default:var
J=a[1],K=m(d+1|0,c+1|0,a[3]),L=m(d,c,J);return h(l[35],L,0,K)}}var
H=b(g[1],p),I=b(g[1],v),J=bI(function(c){var
d=mX(u,c),e=a(n[4],GP,d),f=b(ca[1][6],e);return b(l[11],f)},c),L=b(g[9],F),M=b(g[9],v),N=m(b(g[1],p),0,c);function
O(a){return[0,[0,a[1]],a[2]]}var
P=m3(H,a(g[17],O,p),N);function
Q(a){return[0,[0,a[1]],a[2]]}return[0,m3(I,a(g[17],Q,v),P),M,L,J]}function
m6(g,f){var
d=f,c=g;for(;;){if(c){var
e=c[1],h=c[2],i=e[3],j=e[2],k=b(ca[1][6],e[1]),m=a(m4[4],k,0),d=F(l[48],m,j,i,d),c=h;continue}return d}}var
g4=[f,function(a){return Z(GS,GR,GQ)}],g5=[f,function(a){return Z(GV,GU,GT)}],g6=[f,function(a){return Z(GY,GX,GW)}],g7=[f,function(a){return Z(G1,G0,GZ)}];function
g8(c,a){if(typeof
a==="number"){var
d=j(g6),h=[0,c],m=k===d?g6[1]:f===d?b(i[2],g6):g6;return b(l[23],[0,m,h])}else{if(0===a[0]){var
n=[0,c,a[1]],e=j(g5),o=k===e?g5[1]:f===e?b(i[2],g5):g5;return b(l[23],[0,o,n])}var
p=a[2],q=a[1],r=g8(c,a[3]),s=[0,c,g8(c,q),p,r],g=j(g4),t=k===g?g4[1]:f===g?b(i[2],g4):g4;return b(l[23],[0,t,s])}}function
m7(a){if(a){var
c=a[1][1],d=0,e=function(d,a){var
e=a[1];return eN(c,b(A[1],a[2]),e,d)};return h(g[20],e,d,a)}return 0}function
er(a){if(typeof
a==="number"){var
c=j(gp);return k===c?gp[1]:f===c?b(i[2],gp):gp}else
switch(a[0]){case
0:var
n=a[1],o=er(a[2]),p=[0,ds(L,a1,n),o],d=j(gq),q=k===d?gq[1]:f===d?b(i[2],gq):gq;return b(l[23],[0,q,p]);case
1:var
r=a[1],s=er(a[2]),t=[0,ds(L,a1,r),s],e=j(gr),u=k===e?gr[1]:f===e?b(i[2],gr):gr;return b(l[23],[0,u,t]);case
2:var
v=a[3],w=a[2],x=a[1],g=j(ci),y=k===g?ci[1]:f===g?b(i[2],ci):ci,z=i$(y,er,v),A=ds(L,a1,w),B=[0,ds(L,a1,x),A,z],h=j(gs),C=k===h?gs[1]:f===h?b(i[2],gs):gs;return b(l[23],[0,C,B]);default:var
D=a[1],E=er(a[2]),F=[0,bC(D),E],m=j(gt),G=k===m?gt[1]:f===m?b(i[2],gt):gt;return b(l[23],[0,G,F])}}function
m8(a){return er(a)}function
bF(b,a){return V(n[1],b,G2,co,a[1],gU,a[2])}function
cp(c,b){if(typeof
b==="number")return a(n[1],c,G3);else
switch(b[0]){case
0:var
d=b[2],e=b[1],f=function(a,b){return bX(co,a,b)};return V(n[1],c,G4,f,e,cp,d);case
1:var
g=b[2],h=b[1],i=function(a,b){return bX(co,a,b)};return V(n[1],c,G5,i,h,cp,g);case
2:var
j=b[3],k=b[2],l=b[1],m=function(a,c){function
b(c,a){if(a){var
d=a[2],e=a[1];return d?V(n[1],c,FZ,cp,e,b,d):F(n[1],c,F0,cp,e)}return 0}return V(n[1],a,F1,G7,b,c,G6)},o=function(a,b){return bX(co,a,b)},p=function(a,b){return bX(co,a,b)};return nz(n[1],c,G8,p,l,o,k,m,j);default:return V(n[1],c,G9,gU,b[1],cp,b[2])}}function
m9(a,j,i,h,b){if(b){var
k=b[1],l=k[2],m=k[1],c=m9(a,j,i,h,b[2]),d=c[3],e=c[2],f=c[1];if(je(a[1],a[2],l))try{var
g=m1(a,j,e,d,l),n=[0,[0,[0,m,g[1]],f],g[2],g[3]];return n}catch(a){a=q(a);if(a===M)return[0,f,e,d];throw a}return[0,f,e,d]}return[0,0,i,h]}function
m_(d,c,h,g,f){var
a=m1(d,c,h,b(c9[4],0),f),i=a[1],e=m9(d,c,a[2],a[3],g);return[0,e[1],i,e[2]]}var
g9=[f,function(l){var
a=j(ci),e=k===a?ci[1]:f===a?b(i[2],ci):ci,c=j(L),g=k===c?L[1]:f===c?b(i[2],L):L,d=j(L),h=k===d?L[1]:f===d?b(i[2],L):L;return[0,h,g,a1,e,m8]}],g_=[f,function(m){function
e(a){return ds(aP,cU,a)}var
a=j(a$),g=k===a?a$[1]:f===a?b(i[2],a$):a$,c=j(aP),h=k===c?aP[1]:f===c?b(i[2],aP):aP,d=j(aP),l=k===d?aP[1]:f===d?b(i[2],aP):aP;return[0,l,h,cU,g,e]}];function
G_(a,m,h,g,e){var
n=[0,a[2]],c=j(cn),o=k===c?cn[1]:f===c?b(i[2],cn):cn,d=b(l[23],[0,o,n]),p=a[3],q=a[2],r=m2(d,function(a){return mQ(q,p,a)},e),s=m7(g),t=g8(a[1],s);function
u(g){var
o=b(aC[32][5],g),q=[0,a[1]],c=j(g7),n=0,p=[0,[0,G$,m,h],0],s=k===c?g7[1]:f===c?b(i[2],g7):g7,u=[0,[0,Ha,t,b(l[23],[0,s,q])],p],e=j(cm),v=[0,d],w=k===e?cm[1]:f===e?b(i[2],cm):cm,x=m6([0,[0,Hb,r,b(l[23],[0,w,v])],u],o),y=[0,b(ba[54],x),n];return b(_[57][22],y)}return b(cW[67][7],u)}function
cq(c,a){if(typeof
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
d=a[1],f=c[1],g=cq(c[2],a[2]);return[2,cq(f,d),g]}break;case
3:if(typeof
a!=="number"&&3===a[0]){var
h=a[1],i=c[1],j=cq(c[2],a[2]);return[3,cq(i,h),j]}break;case
4:if(typeof
a!=="number"&&4===a[0])return[4,cq(c[1],a[1])];break;default:if(typeof
a!=="number"&&5===a[0]){var
k=a[2],l=a[1],m=c[1],n=cq(c[3],a[3]);return[5,cq(m,l),k,n]}}return b(e[3],Hh)}var
jg=[bc,Hi,bb(0)];function
m$(b,a){var
c=[0,a,0];function
d(c,b){var
d=b[2],e=b[1],a=c[2],f=c[1];if(typeof
a!=="number"&&0===a[0])return[0,e,d];return[0,[5,a,[0,f],e],[0,f,d]]}return h(g[21],d,b,c)}function
Hj(B,z,y,m,ak,Z,Y,aj){var
o=m$(Z,Y)[1],F=b(c9[4],0),G=dK(function(c,b){return a(c9[3],c,b[1])},o,F),H=1+b(c9[5],G)|0,C=b(A[1],H),D=b(z,a(B,C,o)),p=D[1],_=D[2];function
r(f){if(f){var
h=f[1],d=r(f[2]);if(typeof
d==="number")return 0;else{if(0===d[0]){var
n=d[1],j=function(a){return a[1]},k=a(g[17],j,h),l=[0,b(m[2],0),k],c=b(m[3],l),e=typeof
c==="number"?0:0===c[0]?[0,[0,c[1],m]]:[1,c[1]];return typeof
e==="number"?0:0===e[0]?[0,[0,e[1],n]]:[1,[0,e[1],h]]}var
i=d[1];return[1,[0,i[1],i[2]]]}}return Hc}var
c=r(p);if(typeof
c==="number")return 0;else{if(0===c[0]){var
E=c[1],$=a(g[47],p,E),aa=cF[1],ac=function(c,b){return a(cF[4],b[1],c)},ad=h(g[20],ac,aa,_),af=function(e,c){var
d=c[2],f=c[1],i=cF[1],j=b(d[2][4],d[1]);function
k(c,b){var
d=a(g[7],f,c)[2][1];return a(cF[4],d,b)}var
l=h(t[15],k,j,i);return a(cF[7],e,l)},ag=h(g[20],af,ad,$),s=j(cb),M=k===s?cb[1]:f===s?b(i[2],cb):cb,N=function(a){return b(l[23],[0,M,[0,a]])},O=function(b,a){return h(l[35],b,0,a)},u=j(a7),P=k===u?a7[1]:f===u?b(i[2],a7):a7,Q=function(c,a){return b(l[23],[0,P,[0,c,a]])},v=j(a6),R=k===v?a6[1]:f===v?b(i[2],a6):a6,S=function(c,a){return b(l[23],[0,R,[0,c,a]])},T=function(b,a){return a[2]},w=j(ae),U=k===w?ae[1]:f===w?b(i[2],ae):ae,x=j(a8),V=k===x?a8[1]:f===x?b(i[2],a8):a8,W=[0,V,U,T,S,Q,O,N],X=1,d=bJ(W,function(b){return a(cF[3],b[1],ag)},X,o),ah=b(z,a(B,C,d))[1],I=a(g[47],p,E),J=function(l){try{var
y=function(c){var
e=c[2],f=c[1],i=b(e[2][4],e[1]);function
d(g,f){var
c=g,b=f;for(;;){if(b){var
e=b[2],h=b[1];if(a(t[3],c,i))return[0,h,d(c+1|0,e)];var
c=c+1|0,b=e;continue}return 0}}return kx(ab,d(0,f),l)},z=a(g[33],y,I),c=z}catch(a){a=q(a);if(a!==K)throw a;b(n[2],Hf);b(e[52],e[28]);var
c=b(e[3],Hg)}var
m=c[2],d=m[2],x=c[1],o=m[1];function
p(b,a){return[0,a[1],b]}var
i=a(g[18],p,l);function
r(d){try{var
f=a(g[7],x,d)[1],c=f}catch(a){a=q(a);if(a[1]!==fb)throw a;var
c=b(e[3],Hd)}return a(g[38],c,i)}try{var
w=a(d[5],o,r),k=w}catch(c){c=q(c);if(!b(fR[13],c))throw c;var
s=function(a){return a[1]},u=a(g[17],s,i),v=[0,b(d[2],0),u],f=b(d[3],v);if(typeof
f==="number")var
h=0;else
if(0===f[0])var
j=f[1],h=1;else
var
h=0;if(!h)var
j=b(e[3],He);var
k=j}return k},L=a(g[17],J,ah),ai=hr(d);return[0,[0,ai,d,i$(y[4],y[5],L)]]}return[1,c[1]]}}function
na(j,i,h,g,f,d,c,a){try{var
k=Hj(j,i,h,g,f,d,c,a);return k}catch(a){a=q(a);if(a===K){b(eT[4],e[28]);b(e[52],e[28]);return 0}throw a}}function
nb(d,c,a){var
e=b(cW[67][3],a);return h(ba[13],d,c,e)}function
Hk(c){var
d=b(aC[32][5],c),e=b(aC[32][4],c),f=a(nc[53],nc[38],e);function
g(a){return nx(Hm[4],0,0,0,0,0,0,Hl,0,f,a,d)}return a(Hn[1],0,g)}var
nd=b(cW[67][7],Hk);function
Hw(q,p,o){var
a=j(cf),c=k===a?cf[1]:f===a?b(i[2],cf):cf,d=j(aw),e=k===d?aw[1]:f===d?b(i[2],aw):aw,g=j(a$),r=k===g?a$[1]:f===g?b(i[2],a$):a$,h=j(cc),s=[0,r],t=k===h?cc[1]:f===h?b(i[2],cc):cc,u=b(l[23],[0,t,s]),m=j(cn),v=[0,c],w=k===m?cn[1]:f===m?b(i[2],cn):cn,n=b(l[23],[0,w,v]),x=m2(n,function(a){return mQ(c,bD,a)},o),y=g8(e,m7(p));function
z(c){var
g=b(aC[32][5],c),h=[0,Z(HA,Hz,Hy),[0,e]],m=[0,[0,HB,y,b(l[23],h)],[0,[0,Hx,q,u],0]],a=j(cm),d=0,o=[0,n],p=k===a?cm[1]:f===a?b(i[2],cm):cm,r=m6([0,[0,HC,x,b(l[23],[0,p,o])],m],g),s=[0,b(ba[54],r),d];return b(_[57][22],s)}return b(cW[67][7],z)}function
jh(d){var
h=aa[1],i=b(aa[5],d[4]),j=a(aa[6],i,h),k=b(fY[47],j),l=a(aa[6],d[3],k),m=aa[7],n=b(aa[8],d[2]),o=a(aa[9],n,m),p=b(g[20],o),e=[0,l,b(a(aa[9],d[1],p),0)],c=l$(e),f=c[4],q=c[1],r=c[2],s=c[3],t=c[5];return[0,e,q,r,s,f,t,function(e,d,c){var
g=a(f,d,c);return function(a){return e[1]?b(g,a):b(c,a)}}]}function
HM(a){return a}var
HN=b(aO[6],HM),HO=b(aa[13],HN),HP=a(aa[9],aa[12],HO),HQ=b(aO[4],x[2]),HR=a(aa[6],aW[34],HQ),HS=jh([0,HP,aa[11],HR,aa[4]]),ne=b(HU[8],HT)?0:[f,function(a){throw jg}];function
H0(m){var
p=m[2],q=m[1],d=j(ne);if(k!==d)if(f===d)b(i[2],ne);var
n=[0,HY,[0,HX,[0,a(e[17],HW,HV[22]),0]]],o=b(HZ[3],0),l=h(g[20],bN[4],o,n),c=kJ(l,[0,l],[0,q,p]);if(0===c[0])return c[1];throw b(e[3],c[1])}var
H2=h(HS[7],CG,H1,H0);function
nf(c,a){return b(H2,[0,c,a])}function
g$(a){switch(a[0]){case
0:return[0,[0,a[1],0]];case
1:var
b=a[1];return[1,b,g$(a[2])];default:var
c=a[2],d=a[1],e=g$(a[3]);return[2,g$(d),c,e]}}function
ng(f,a){var
c=nf(f,a);if(c){var
d=mv(c[1]);return hN(a,d)?[0,d]:(b(e[31],H3),0)}return 0}function
es(e,d,c){function
f(i,h){var
c=i,d=h;for(;;){if(typeof
c!=="number")switch(c[0]){case
0:var
g=b(aA[6],c[1]);return e<=g?a(t[4],g-e|0,d):d;case
2:var
c=c[2];continue;case
3:case
4:var
j=c[1],k=f(c[2],d),c=j,d=k;continue}return d}}return f(c,d)}function
et(a){return es(0,t[1],a)}function
bY(a,d){function
c(a){if(typeof
a!=="number")switch(a[0]){case
0:var
e=b(d,b(aA[6],a[1]));return[0,b(A[4],e)];case
2:var
f=a[1];return[2,f,c(a[2])];case
3:var
g=a[1],h=c(a[2]);return[3,c(g),h];case
4:var
i=a[1],j=c(a[2]);return[4,c(i),j]}return a}return c(a)}function
ji(a){function
d(i,f,e){var
b=i,a=f,c=e;for(;;)if(typeof
a==="number")return c;else
switch(a[0]){case
0:var
j=a[2],k=es(b,c,a[1]),b=b+1|0,a=j,c=k;continue;case
1:var
l=a[2],m=es(b,c,a[1]),b=b+1|0,a=l,c=m;continue;case
2:var
n=a[3],o=a[1],p=es(b,es(b,c,a[2]),o),q=function(c,a){return d(b+1|0,a,c)};return h(g[20],q,p,n);default:var
b=b+3|0,a=a[2];continue}}return d(0,a,t[1])}function
jj(a,e){function
d(c,a){return a<c?a:b(e,a-c|0)+c|0}function
c(b,a){if(typeof
a==="number")return 0;else
switch(a[0]){case
0:var
e=a[1],f=c(b+1|0,a[2]);return[0,bY(e,function(a){return d(b,a)}),f];case
1:var
g=a[1],h=c(b+1|0,a[2]);return[1,bY(g,function(a){return d(b,a)}),h];case
2:var
i=a[3],j=a[2],k=a[1],l=cY(function(a){return c(b+1|0,a)},i),m=bY(j,function(a){return d(b,a)});return[2,bY(k,function(a){return d(b,a)}),m,l];default:var
n=a[1];return[3,n,c(b+3|0,a[2])]}}return c(0,a)}function
eu(d,c){function
e(a){var
b=a[2];return[0,kd(a[1]),b]}return b(d,a(g[17],e,c))}var
H5=b(aa[13],bi[27]),nh=jh([0,H5,aa[10],ab,aa[3]]),H6=b(aa[13],bi[27]),H7=jh([0,H6,aa[11],x[2],aa[4]]);function
H8(a){var
b=a[1],c=a[2],d=b[3],e=b[2];return eu(function(a){return mC(e,d,a)},c)}var
H_=h(nh[7],i6,H9,H8);function
H$(a){var
b=a[1],c=a[2],d=b[3],e=b[2];return eu(function(a){return mD(e,d,a)},c)}var
Ib=h(nh[7],i7,Ia,H$);function
Ic(a){var
b=a[2],c=a[1];return eu(function(a){return ms(c,a)},b)}var
Ie=h(H7[7],i8,Id,Ic);function
If(b,a){return bE(bF,b,a[1])}function
Ig(a,b){return bX(bF,a,b)}var
Ii=[0,Ih,i5,function(a){var
b=a[2],c=a[1];return eu(function(a){return iY(c,a)},b)},et,bY,Ig,If];function
Ij(b,a){return bE(bF,b,a[1])}function
Ik(a,b){return bX(bF,a,b)}var
Im=[0,Il,i5,function(a){var
b=a[2],c=a[1];return eu(function(a){return iY(c,a)},b)},et,bY,Ik,Ij];function
In(b,a){return bE(bF,b,a[1])}var
ni=[0,Io,i5,Ie,et,bY,function(a,b){return bX(bF,a,b)},In];function
nj(b,a){function
c(b,a){return bE(bF,b,a[1])}function
d(a,b){return bX(bF,a,b)}function
e(a){return ng(a[1],a[2])}return[0,Ip,function(c){return[0,b,a]},e,et,bY,d,c]}function
nk(b,a){function
c(b,a){return bE(bF,b,a[1])}function
d(a,b){return bX(bF,a,b)}function
e(a){return ng(a[1],a[2])}return[0,Iq,function(c){return[0,b,a]},e,et,bY,d,c]}function
nl(d,c){function
f(b,a){return bE(co,b,a[1])}function
h(h){var
i=h[2],k=h[1];function
j(a){var
b=a[2];return[0,g$(a[1]),b]}var
d=nf(k,a(g[17],j,i));if(d)var
f=mx(d[1]),c=kf(i,f)?[0,f]:(b(e[31],H4),b(e[52],e[28]),0);else
var
c=0;if(typeof
c!=="number"&&0===c[0])return[0,[0,c[1],0]];return 0}return[0,Ir,function(a){return[0,d,c]},h,ji,jj,cp,f]}var
It=[0,Is,mG,H_,ji,jj,cp,function(b,a){return bE(co,b,a[1])}],Iv=[0,Iu,mG,Ib,ji,jj,cp,function(b,a){return bE(co,b,a[1])}];function
Iw(a){var
d=b(aC[32][5],a),e=b(aC[32][3],a);if(je(b(aC[32][4],a),e,d))return _[57][2];var
c=j(ae),g=k===c?ae[1]:f===c?b(i[2],ae):ae;return b(ba[oA],g)}var
nm=b(cW[67][7],Iw);function
cr(aw,av,au,o,n,at,as){function
c(c){var
D=b(aC[32][3],c),E=b(aC[32][5],c),F=b(aC[32][13],c);try{var
d=[0,b(aC[32][4],c),D],p=m_(d,aw,gW(d),F,E),w=p[3][1],L=p[2],M=p[1],x=j(o),r=k===x?o[1]:f===x?b(i[2],o):o,y=j(n),N=k===y?n[1]:f===y?b(i[2],n):n,s=na(av,au,r,at,w,M,L,d);if(typeof
s==="number"){b(e[52],e[28]);var
O=b(cV[3],Ht),t=a(_[57][4],0,O)}else
if(0===s[0])var
u=s[1],z=u[2],P=u[3],Q=u[1],m=m5(d,N,z),v=m[3],R=m[4],S=m[2],T=m[1],A=function(a){return b(ba[2],a[1])},U=a(g[17],A,v),V=b(_[57][22],U),W=a(g[17],A,S),X=b(_[57][22],W),Y=b(ca[1][6],Hu),B=nb(ca[1][10][1],Y,c),Z=function(a){var
c=a[2];return[0,b(l[11],a[1]),c]},$=a(g[17],Z,v),ab=[0,r[4]],C=j(cc),aa=0,ac=k===C?cc[1]:f===C?b(i[2],cc):cc,ad=[0,nd,[0,X,[0,V,[0,G_(r,P,b(l[23],[0,ac,ab]),$,R),aa]]]],ae=b(_[57][22],ad),af=jf(d,z)[1],ag=b(g[9],af),ah=function(b){return a(g[7],w,b[2]-1|0)},ai=a(g[17],ah,v),aj=a(e[26],ag,ai),ak=a(_[57][3],ae,as),al=a(g[17],l[11],Q),am=a(e[26],aj,al),an=[0,b(l[11],B),am],ao=b(l[41],an),ap=b(ba[46],ao),aq=h(ba[140],[0,B],T,ak),t=a(_[57][3],aq,ap);else
var
ar=b(cV[3],Hv),t=a(_[57][4],0,ar);return t}catch(c){c=q(c);if(c===fT){var
G=b(cV[3],Ho);return a(_[57][4],0,G)}if(c===jg){b(e[52],e[28]);var
H=a(e[17],Hq,Hp),I=a(e[17],Hr,H),J=a(e[17],Hs,I),K=b(cV[3],J);return a(_[57][4],0,K)}throw c}}var
d=b(cW[67][7],c);return a(_[57][3],nm,d)}function
ha(ay,ax){var
h=[f,function(m){function
e(a){return ds(aP,cU,a)}var
a=j(a$),g=k===a?a$[1]:f===a?b(i[2],a$):a$,c=j(cf),h=k===c?cf[1]:f===c?b(i[2],cf):cf,d=j(aw),l=k===d?aw[1]:f===d?b(i[2],aw):aw;return[0,l,h,cU,g,e]}];function
c(c){var
D=b(aC[32][3],c),E=b(aC[32][5],c),G=b(aC[32][13],c);try{var
d=[0,b(aC[32][4],c),D],n=m_(d,GG,gW(d),G,E),t=n[2],u=n[1],v=n[3][1],w=j(h),M=k===w?h[1]:f===w?b(i[2],h):h,N=function(a){var
b=a[2],c=a[1];return[0,c,be(function(a){return eM(aJ,a)},b)]},O=a(g[17],N,u),P=be(function(a){return eM(aJ,a)},t),o=na(function(b,a){return a},c7,M,ay,v,O,P,d);if(typeof
o==="number")var
s=0;else
if(0===o[0])var
p=o[1],R=p[3],S=p[2],T=p[1],U=function(b){return a(g[31],b[1],T)},y=m$(a(g[35],U,u),t),V=y[2],z=cq(S,y[1]),A=j(g3),W=k===A?g3[1]:f===A?b(i[2],g3):g3,m=m5(d,W,z),r=m[3],X=m[4],Y=m[2],Z=m[1],B=function(a){return b(ba[2],a[1])},$=a(g[17],B,r),aa=b(_[57][22],$),ab=a(g[17],B,Y),ac=b(_[57][22],ab),ad=function(b){return[0,a(HJ[1],0,[1,[0,b]])]},ae=b(ca[1][6],HK),C=nb(ca[1][10][1],ae,c),af=function(a){var
c=a[2];return[0,b(l[11],a[1]),c]},ag=[0,nd,[0,ac,[0,aa,[0,Hw(R,a(g[17],af,r),X),0]]]],ah=b(_[57][22],ag),ai=jf(d,z)[1],aj=b(g[9],ai),ak=function(b){return a(g[7],v,b[2]-1|0)},al=a(g[17],ak,r),am=a(e[26],aj,al),an=a(_[57][3],ah,ax),ao=[0,b(l[11],C),am],ap=b(l[41],ao),aq=[0,b(ba[46],ap),0],ar=a(g[17],l[11],V),as=[0,b(ba[148],ar),aq],at=[0,an,[0,b(_[57][22],as),0]],au=ad(C),av=F(ba[ov],1,HL,au,Z),x=a(_[57][21],av,at),s=1;else
var
s=0;if(!s){b(e[52],e[28]);var
Q=b(cV[3],HI),x=a(_[57][4],0,Q)}return x}catch(c){c=q(c);if(c===fT){var
H=b(cV[3],HD);return a(_[57][4],0,H)}if(c===jg){b(e[52],e[28]);var
I=a(e[17],HF,HE),J=a(e[17],HG,I),K=a(e[17],HH,J),L=b(cV[3],K);return a(_[57][4],0,L)}throw c}}var
d=b(cW[67][7],c);return a(_[57][3],nm,d)}function
Ix(b,a){return a}function
nn(a){return cr(gZ,Ix,c7,g_,g2,Ii,a)}function
jk(a){var
b=nj(Iy,[0,a]);function
c(b,a){return a}return function(a){return cr(gZ,c,c7,g_,g2,b,a)}}function
no(a){return ha(Im,a)}function
jl(a){var
b=nk(Iz,[0,a]);return function(a){return ha(b,a)}}function
jm(a){var
b=nl(IA,[0,a]);function
c(b,a){return a}return function(a){return cr(gY,c,dM,g9,g1,b,a)}}var
IC=nl(IB,0);function
ID(b,a){return a}function
np(a){return cr(gY,ID,dM,g9,g1,IC,a)}var
IF=nj(IE,0);function
IG(b,a){return a}function
nq(a){return cr(gZ,IG,c7,g_,g2,IF,a)}var
II=nk(IH,0);function
nr(a){return ha(II,a)}function
IJ(b,a){return a}function
ns(a){return cr(gY,IJ,dM,g9,g1,It,a)}function
IK(b,a){return a}function
nt(a){return cr(gY,IK,dM,g9,g1,Iv,a)}function
nu(a){return ha(ni,a)}function
IL(b,a){return a}function
nv(a){return cr(gZ,IL,c7,g_,g2,ni,a)}aF(1059,[0,jm,jk,jl,ns,nt,nu,nv,np,nq,nr,nn,no,m8],"Micromega_plugin__Coq_micromega");b(IM[9],aQ);var
IN=0,IP=[0,[0,IO,function(a){return ba[56]}],IN];U(a2[8],aQ,IQ,0,0,IP);var
IR=0;function
IS(d,c){var
e=a(aD[24],c,d);return b(jm(-1),e)}var
IU=[0,[0,[0,IT,[1,[5,b(ai[16],aE[9])],0]],IS],IR];function
IV(e,d,c){var
f=a(aD[24],c,d);return b(jm(e),f)}var
IW=[1,[5,b(ai[16],aE[9])],0],IY=[0,[0,[0,IX,[1,[5,b(ai[16],jn[6])],IW]],IV],IU];U(a2[8],aQ,IZ,0,0,IY);var
I0=0;function
I1(c,b){return ns(a(aD[24],b,c))}var
I3=[0,[0,[0,I2,[1,[5,b(ai[16],aE[9])],0]],I1],I0];U(a2[8],aQ,I4,0,0,I3);var
I5=0;function
I6(c,b){return nt(a(aD[24],b,c))}var
I8=[0,[0,[0,I7,[1,[5,b(ai[16],aE[9])],0]],I6],I5];U(a2[8],aQ,I9,0,0,I8);var
I_=0;function
I$(c,b){return nu(a(aD[24],b,c))}var
Jb=[0,[0,[0,Ja,[1,[5,b(ai[16],aE[9])],0]],I$],I_];U(a2[8],aQ,Jc,0,0,Jb);var
Jd=0;function
Je(c,b){return nv(a(aD[24],b,c))}var
Jg=[0,[0,[0,Jf,[1,[5,b(ai[16],aE[9])],0]],Je],Jd];U(a2[8],aQ,Jh,0,0,Jg);var
Ji=0;function
Jj(c,b){return np(a(aD[24],b,c))}var
Jl=[0,[0,[0,Jk,[1,[5,b(ai[16],aE[9])],0]],Jj],Ji];U(a2[8],aQ,Jm,0,0,Jl);var
Jn=0;function
Jo(c,b){return nq(a(aD[24],b,c))}var
Jq=[0,[0,[0,Jp,[1,[5,b(ai[16],aE[9])],0]],Jo],Jn];U(a2[8],aQ,Jr,0,0,Jq);var
Js=0;function
Jt(c,b){return nr(a(aD[24],b,c))}var
Jv=[0,[0,[0,Ju,[1,[5,b(ai[16],aE[9])],0]],Jt],Js];U(a2[8],aQ,Jw,0,0,Jv);var
Jx=0;function
Jy(c,b){return nn(a(aD[24],b,c))}var
JA=[0,[0,[0,Jz,[1,[5,b(ai[16],aE[9])],0]],Jy],Jx];U(a2[8],aQ,JB,0,0,JA);var
JC=0;function
JD(c,b){return no(a(aD[24],b,c))}var
JF=[0,[0,[0,JE,[1,[5,b(ai[16],aE[9])],0]],JD],JC];U(a2[8],aQ,JG,0,0,JF);var
JH=0;function
JI(d,c){var
e=a(aD[24],c,d);return b(jl(-1),e)}var
JK=[0,[0,[0,JJ,[1,[5,b(ai[16],aE[9])],0]],JI],JH];function
JL(e,d,c){var
f=a(aD[24],c,d);return b(jl(e),f)}var
JM=[1,[5,b(ai[16],aE[9])],0],JO=[0,[0,[0,JN,[1,[5,b(ai[16],jn[6])],JM]],JL],JK];U(a2[8],aQ,JP,0,0,JO);var
JQ=0;function
JR(d,c){var
e=a(aD[24],c,d);return b(jk(-1),e)}var
JT=[0,[0,[0,JS,[1,[5,b(ai[16],aE[9])],0]],JR],JQ];function
JU(e,d,c){var
f=a(aD[24],c,d);return b(jk(e),f)}var
JV=[1,[5,b(ai[16],aE[9])],0],JX=[0,[0,[0,JW,[1,[5,b(ai[16],jn[6])],JV]],JU],JT];U(a2[8],aQ,JY,0,0,JX);aF(1066,[0],"Micromega_plugin__G_micromega");return}
