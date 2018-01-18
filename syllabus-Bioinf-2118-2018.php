<HTML>
<BODY><p>
<h4><b>Syllabus:</b></h4>
<table width="801" border="0">
  <tr>
    <td width="58">NOTES: </td>
    <td width="727">The class number does not always correspond to the document
    number. </td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>This syllabus may undergo some in-flight modifications.</td>
  </tr>
  <tr>
    <td>&nbsp;</td>
    <td>Some of the documents may receive some improvements with time. Occasionally, visit the listing below &amp; sort by date.</td>
  </tr>
</table>
<p><br>
</p>
<TABLE border="1"> 
  <tr bgcolor="#E6E6E6">
   <td><b>Session</b><br>&nbsp;</td>
   <td><b>Date</b><br>&nbsp;</td>
   <td><b>Topic</b><br>&nbsp;</td>
   <td><b>Notes and Handouts</b><br>&nbsp;</td>
   <td><b>HW Assignment</b><br>&nbsp;</td>
 </tr>
<?php

$bgcolorArray = array("\"#FEEEEE\"", "\"#E6E6E6\"");
$dayArray = array("<br>Tuesday", "<br>Thursday");
$dates = array(
"09 Jan", "11 Jan", "16 Jan", "18 Jan", "23 Jan", "25 Jan", "30 Jan",
"01 Feb", "06 Feb", "08 Feb", "13 Feb", "15 Feb", "20 Feb", "22 Feb",
"27 Feb", "01 Mar", "06 Mar", "08 Mar", "13 Mar", "15 Mar", "20 Mar",
"22 Mar", "27 Mar", "29 Mar",
"03 Apr", "05 Apr", "10 Apr", "12 Apr", "17 Apr", "19 Apr", "24 Apr",
"26 Apr");

$nClasses = count($dates);
$hw = '';
$entries = array(
array(
   'Introduction to probability, likelihoods, and notation.',
   '<a
       href="N-files-for-handouts/N01a-Introduction%20to%20Probability.docx">N
       01a</a>&nbsp		
    <a
       href="N-files-for-handouts/N01b-concepts%20list.docx">
     N 01b</a>&nbsp
   <br>
   <a href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=e1e2f75b-2605-47c2-bbb9-a863016f6f5d">Panopto Recording (Jan 9)</a>',
   '<a
     href="_Assignments-2118/Assignment%201,%20Bioinf%202118.docx">Homework
     1</a> assigned.<br>
     Read Dalgaard Chapter 1.1.1-1.2.7<br>
     Read Shahbaba Chapter 1.1-1.5&nbsp;'
), array(
   '<br>Introduction to R, Rstudio, and Rmarkdown. Tables in R.
    <br>Conditional, marginal, and joint probabilities.
    <br>Conditional probability, causality, and Bayes ideas.<br><br> ',
   '
    <a href="N-files-for-handouts/N02-More Probability.docx">N 02</a>&nbsp
   ',
   '&nbsp;'
), array(
   'Random variables  and distributions : <br> Discrete random variables&nbsp;',
   '<a href="N-files-for-handouts/N03-Discrete Random Variables.docx">N 03</a>
   <br>
   <a href="https://pitt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=1e367d62-434a-4ae1-b572-af56ef0cad35">
   	Panopto Recording (Jan 16)</a>
   <br>
    <a href="N-files-for-handouts/pascalsTriangle.docx">pascalsTriangle.docx</a>',
   'Homework 1 due.&nbsp;   <br>
   <a
     href="_Assignments-2118/Assignment%202,%20Bioinf%202118.docx">
     Homework 2</a> assigned.<br>&nbsp;
   '
)
);
for($i=0; $i<$nClasses; $i++) {
 $bgcolor = $bgcolorArray[$i & 1];  
 $day = $dayArray[($i & 1)];  
 echo " <tr bgcolor=" . $bgcolor . ">";
 echo " <td><b>" . ($i + 1) . "</b></td>";
 echo " <td><b>" . $dates[$i] . " " . $day .  "</b></td>";
 echo " <td><b>" . $entries[$i][0] . "</b></td>";
 echo " <td><b>" . $entries[$i][1]. "</b></td>";
 echo " <td><b>" . $entries[$i][2] . "</b></td>";
 echo " </tr>";

}
?>
</table>


<hr>

</BODY>
</HTML>
