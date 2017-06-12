package com.ubs.opsit.interviews

class TimeConverterImpl extends TimeConverter {

  private def convertTopHours(time: TimeVO) = convertToLightSign(topLineNumberOfOnLamp(time.hours), 4, "R")

  private def convertBottomHours(time: TimeVO) = convertToLightSign(time.hours % 5, 4, "R")

  private def convertTopMinutes(time: TimeVO) = convertToLightSign(topLineNumberOfOnLamp(time.minutes), 11, "Y").replaceAll("YYY", "YYR")

  private def convertBottomMinutes(time: TimeVO) = convertToLightSign(time.minutes % 5, 4, "Y")

  private def convertSeconds(time: TimeVO) = if (time.seconds % 2 == 0) "Y" else "O"

  private def parseTime(aTime: String): TimeVO = {
    val split = aTime.split(":").map(_.toInt)
    TimeVO.apply(split(0), split(1), split(2))
  }

  private def topLineNumberOfOnLamp(timeAmount: Int): Int = (timeAmount - (timeAmount % 5)) / 5

  private def convertToLightSign(lightLampNumber: Int, totalNumberLamps: Int, lightSignal: String) = {
    var result = ""
    for (index <- 0 until totalNumberLamps) {
      if (index < lightLampNumber) result += lightSignal
      else result += "O"
    }
    result
  }

  override def convertTime(aTime: String): String = {
    val separator = sys.props("line.separator")
    val time = parseTime(aTime)
    convertSeconds(time) + separator +
      convertTopHours(time) + separator +
      convertBottomHours(time) + separator +
      convertTopMinutes(time) + separator +
      convertBottomMinutes(time)
  }
}
