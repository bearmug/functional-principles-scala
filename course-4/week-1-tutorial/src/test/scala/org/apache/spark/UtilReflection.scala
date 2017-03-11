package org.apache.spark

object UtilReflection {

  def amend(name: String, value: Any): Unit = {
    val field = org.apache.spark.util.Utils.getClass.getDeclaredField(name)
    field.setAccessible(true)
    field.set(org.apache.spark.util.Utils, value)
  }
}
