notice=new Array;
cstr=new Array;

cstr['IEEE']="\
© ";

cstr['ACM']="\
ACM COPYRIGHT NOTICE. Copyright © ";

notice['IEEE']=" IEEE. Personal use of this material is permitted. However, permission to reprint/republish this material for advertising or promotional purposes or for creating new collective works for resale or redistribution to servers or lists, or to reuse any copyrighted component of this work in other works must be obtained from the IEEE.<br><br>\\This material is presented to ensure timely dissemination of scholarly and technical work. Copyright and all rights therein are retained by authors or by other copyright holders. All persons copying this information are expected to adhere to the terms and constraints invoked by each author's copyright. In most cases, these works may not be reposted without the explicit permission of the copyright holder.<br><br>\
";

notice['ACM']=" by the Association for Computing Machinery, Inc. Permission to make digital or hard copies of part or all of this work for personal or classroom use is granted without fee provided that copies are not made or distributed for profit or commercial advantage and that copies bear this notice and the full citation on the first page.  Copyrights for components of this work owned by others than ACM must be honored. Abstracting with credit is permitted.  To copy otherwise, to republish, to post on servers, or to redistribute to lists, requires prior specific permission and/or a fee.  Request permissions from Publications Dept., ACM, Inc., fax +1 (212) 869-0481, or permissions@acm.org.<br><br>\
";

function c(holder,year,docfilename) {
    document.write(cstr[holder]+(""+year)+notice[holder]+'<a href="'+docfilename+'">'+docfilename+'</a>');
}
