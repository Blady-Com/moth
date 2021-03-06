﻿<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:dyn="http://exslt.org/dynamic" xmlns:ext="http://exslt.org/common" version="1.0">
<xsl:output method="text"/>

<xsl:template match="/">
  <xsl:apply-templates select="platform/contexts" mode="os_task_id"/>
  <xsl:apply-templates select="platform/contexts" mode="os_task_ro"/>
</xsl:template>

<xsl:template match="contexts" mode="os_task_ro">
  <xsl:text>/*****************************************************************************&#xa;</xsl:text>
  <xsl:text> *&#xa;</xsl:text>
  <xsl:text> * Static task configuration generated by task_config.xsl&#xa;</xsl:text>
  <xsl:text> *&#xa;</xsl:text>
  <xsl:text> *****************************************************************************/&#xa;</xsl:text>
  <xsl:text>&#xa;</xsl:text>
  <xsl:text>#include &lt;os.h&gt;&#xa;</xsl:text>
  <xsl:text>#include &lt;os_task_id.h&gt;&#xa;</xsl:text>
  <xsl:text>&#xa;</xsl:text>
  <xsl:text>__attribute__((section(".rodata")))&#xa;</xsl:text>
  <xsl:text>os_task_ro_t const os_task_ro[CONFIG_MAX_TASK_COUNT] = {&#xa;</xsl:text>
  <xsl:apply-templates select="context" mode="os_task_ro"/>
  <xsl:text>};&#xa;</xsl:text>
</xsl:template>

<xsl:template match="context" mode="os_task_ro">
  <xsl:text>  { /* </xsl:text>
  <xsl:value-of select="@name"/>
  <xsl:text> */&#xa;</xsl:text>
  <xsl:text>    </xsl:text>
  <xsl:value-of select="priority"/>
  <xsl:text>, /* priority */&#xa;</xsl:text>
  <xsl:text>    0</xsl:text>
  <xsl:apply-templates select="mbx" mode="os_task_ro"/>
  <xsl:text>, /* mbx_permission */&#xa;</xsl:text>
  <xsl:apply-templates select="virtual_ref" mode="os_task_ro"/>
  <xsl:text>  },&#xa;</xsl:text>
</xsl:template>

<xsl:template match="mbx" mode="os_task_ro">
  <xsl:apply-templates select="permission" mode="os_task_ro"/>
</xsl:template>

<xsl:template match="permission" mode="os_task_ro">
  <xsl:variable name="smallcase" select="'abcdefghijklmnopqrstuvwxyz'" />
  <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />
  <xsl:variable name="task" select="."/>
  <xsl:text> | 1 &lt;&lt; OS_</xsl:text>
  <xsl:value-of select="translate($task, $smallcase, $uppercase)" />
  <xsl:text>_TASK_ID</xsl:text>
</xsl:template>

<xsl:template match="virtual_ref" mode="os_task_ro">
  <xsl:variable name="ref1" select="./text()"/>
  <xsl:apply-templates select="dyn:evaluate($ref1)" mode="os_task_ro"/>
</xsl:template>

<xsl:template match="virtual" mode="os_task_ro">
  <xsl:if test="@name != 'kernel'">
    <xsl:text>    </xsl:text>
    <xsl:apply-templates select="virtual_map[@name = 'text']" mode="os_task_ro"/>
    <xsl:text>    </xsl:text>
    <xsl:apply-templates select="virtual_map[@name = 'bss']" mode="os_task_ro"/>
    <xsl:text>    </xsl:text>
    <xsl:apply-templates select="virtual_map[@name = 'stack']" mode="os_task_ro"/>
  </xsl:if>
</xsl:template>

<xsl:template match="virtual_map" mode="os_task_ro">
  <xsl:text>{ </xsl:text>
  <xsl:apply-templates select="." mode="vaddress"/>
  <xsl:text> /* virtual address */, </xsl:text>
  <xsl:apply-templates select="." mode="size"/>
  <xsl:text> /* size */}, /* </xsl:text>
  <xsl:value-of select="@name"/>
  <xsl:text> */&#xa;</xsl:text>
</xsl:template>

<xsl:template match="contexts" mode="os_task_id">
  <xsl:document href="os_task_id.h" method="text">
    <xsl:text>/*****************************************************************************&#xa;</xsl:text>
    <xsl:text> *&#xa;</xsl:text>
    <xsl:text> * List of task IDs generated by task_config.xsl&#xa;</xsl:text>
    <xsl:text> *&#xa;</xsl:text>
    <xsl:text> *****************************************************************************/&#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>#ifndef __OS_TASK_ID_H_&#xa;</xsl:text>
    <xsl:text>#define __OS_TASK_ID_H_&#xa;</xsl:text>
    <xsl:text>&#xa;</xsl:text>
    <xsl:apply-templates select="context" mode="os_task_id"/>
    <xsl:text>&#xa;</xsl:text>
    <xsl:text>#endif // __OS_TASK_ID_H_&#xa;</xsl:text>
  </xsl:document>
</xsl:template>

<xsl:template match="context" mode="os_task_id">
  <xsl:variable name="smallcase" select="'abcdefghijklmnopqrstuvwxyz'" />
  <xsl:variable name="uppercase" select="'ABCDEFGHIJKLMNOPQRSTUVWXYZ'" />
  <xsl:variable name="task" select="@name"/>
  <xsl:text>#define OS_</xsl:text>
  <xsl:value-of select="translate($task, $smallcase, $uppercase)" />
  <xsl:text>_TASK_ID </xsl:text>
  <xsl:value-of select="position() - 1"/>
  <xsl:text>&#xa;</xsl:text>
</xsl:template>

<xsl:template match="virtual" mode="name">
  <xsl:value-of select="@name"/>
</xsl:template>

<xsl:template match="virtual" mode="vaddress">
  <xsl:apply-templates select="virtual_map[1]" mode="vaddress"/>
</xsl:template>

<xsl:template match="virtual" mode="vsize">
    <xsl:text>0x40000</xsl:text>
</xsl:template>

<xsl:template match="virtual_map" mode="vaddress">
  <xsl:choose>
    <xsl:when test="./address">
      <xsl:value-of select="./address"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:variable name="hexaddress">
        <xsl:apply-templates select="preceding-sibling::virtual_map[ 1] " mode="vaddress"/>
      </xsl:variable>
      <xsl:variable name="hexsize">
        <xsl:apply-templates select="preceding-sibling::virtual_map[ 1] " mode="size"/>
      </xsl:variable>
      <xsl:variable name="decaddress">
        <xsl:call-template name="toDecimal">
          <xsl:with-param name="num" select="$hexaddress"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:variable name="decsize">
        <xsl:call-template name="toDecimal">
          <xsl:with-param name="num" select="$hexsize"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:call-template name="toHex">
        <xsl:with-param name="num" select="$decsize + $decaddress"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="virtual_map" mode="size">
  <xsl:variable name="size">
  <xsl:choose>
    <xsl:when test="./size">
      <xsl:value-of select="./size"/>
    </xsl:when>
    <xsl:when test="./physical_ref">
      <xsl:variable name="ref1" select="./physical_ref/text()"/>
      <xsl:apply-templates select="dyn:evaluate($ref1)" mode="size"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>0</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  </xsl:variable>
  <xsl:variable name="decsize">
    <xsl:call-template name="toDecimal">
      <xsl:with-param name="num" select="$size"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:call-template name="toHex">
    <xsl:with-param name="num" select="$decsize"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="virtual_map" mode="paddress">
  <xsl:choose>
    <xsl:when test="./physical_ref">
      <xsl:variable name="ref1" select="./physical_ref/text()"/>
      <xsl:apply-templates select="dyn:evaluate($ref1)" mode="paddress"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>0</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="physical_map" mode="size">
  <xsl:choose>
    <xsl:when test="./size[1]">
      <xsl:value-of select="./size[1]"/>
    </xsl:when>
    <xsl:when test="./physical_map">
      <xsl:call-template name="sum_size">
        <xsl:with-param name="objects" select="./physical_map" />
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>0</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="physical_map" mode="paddress">
  <xsl:choose>
    <xsl:when test="address">
      <xsl:value-of select="address"/>
    </xsl:when>
    <xsl:when test="preceding-sibling::physical_map[ 1]">
      <xsl:variable name="hexaddress">
        <xsl:apply-templates select="preceding-sibling::physical_map[ 1] " mode="paddress"/>
      </xsl:variable>
      <xsl:variable name="hexsize">
        <xsl:apply-templates select="preceding-sibling::physical_map[ 1] " mode="size"/>
      </xsl:variable>
      <xsl:variable name="decaddress">
        <xsl:call-template name="toDecimal">
          <xsl:with-param name="num" select="$hexaddress"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:variable name="decsize">
        <xsl:call-template name="toDecimal">
         <xsl:with-param name="num" select="$hexsize"/>
        </xsl:call-template>
      </xsl:variable>
      <xsl:call-template name="toHex">
        <xsl:with-param name="num" select="$decaddress + $decsize"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="ancestor::physical_map[ 1]">
      <xsl:apply-templates select=".." mode="paddress"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>0</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- Utility functions -->

<xsl:template name="toHex">
  <xsl:param name="num"/>
  <xsl:choose>
    <xsl:when test="substring($num,2,1) = 'x'">
      <xsl:value-of select="$num"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>0x</xsl:text>
      <xsl:choose>
        <xsl:when test="$num > 0">
          <xsl:call-template name="num2hex">
            <xsl:with-param name="dec" select="$num"/>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>00000000</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="num2hex">
  <xsl:param name="dec"/>
  <xsl:if test="$dec > 0">
    <xsl:call-template name="num2hex">
      <xsl:with-param name="dec" select="floor($dec div 16)"/>
    </xsl:call-template>
    <xsl:value-of select="substring('0123456789ABCDEF', (($dec mod 16) + 1), 1)"/>
  </xsl:if>
</xsl:template>

<xsl:template name="toDecimal">
  <xsl:param name="num"/>
  <xsl:choose>
  <xsl:when test="substring($num,2,1) = 'x'">
    <xsl:call-template name="hex2num">
      <xsl:with-param name="hex">
        <xsl:value-of select="substring($num,3)"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:when>
  <xsl:otherwise>
    <xsl:value-of select="$num"/>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="hex2num">
  <xsl:param name="hex"/>
  <xsl:param name="num" select="0"/>
  <xsl:param name="MSB" select="translate(substring($hex, 1, 1), 'abcdef', 'ABCDEF')"/>
  <xsl:param name="value" select="string-length(substring-before('0123456789ABCDEF', $MSB))"/>
  <xsl:param name="result" select="16 * $num + $value"/>
  <xsl:choose>
    <xsl:when test="string-length($hex) > 1">
      <xsl:call-template name="hex2num">
        <xsl:with-param name="hex" select="substring($hex, 2)"/>
        <xsl:with-param name="num" select="$result"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$result"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="sum_size">
  <xsl:param name="total" select="0" />
  <xsl:param name="objects"  />
  <xsl:variable name="head" select="$objects[1]" />
  <xsl:variable name="tail" select="$objects[position()>1]" />
  <xsl:variable name="calc">
    <xsl:apply-templates select="$head" mode="size"/>
  </xsl:variable> 
  <xsl:variable name="deccalc">
    <xsl:call-template name="toDecimal">
      <xsl:with-param name="num" select="$calc"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:choose>
    <xsl:when test="not($tail)">
      <xsl:value-of select="$total + $deccalc" />
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="sum_size">
        <xsl:with-param name="total" select="$total + $deccalc" />
        <xsl:with-param name="objects" select="$tail" />
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
   
</xsl:stylesheet>
