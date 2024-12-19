package day9

import scala.annotation.tailrec

def day9a(input: String): Long = {
  val diskMap = input.strip()
  val disk = buildDisk(diskMap)
  compactDisk(disk)
  computeChecksum(disk)
}

def day9b(input: String): Long = {
  val diskMap = input.strip()
  val disk = buildDisk(diskMap)

  // prettyPrintDisk(disk)
  compactDiskWholeFiles(disk)
  // prettyPrintDisk(disk)

  computeChecksum(disk)
}

private def buildDisk(diskMap: String): Array[Int] = {
  val arr = Array.fill(diskSize(diskMap))(-1)
  var cursor = 0
  var currentFileId = 0

  for ((c, i) <- diskMap.zipWithIndex) {
    val inputType = if (i % 2 == 0) then "file" else "free"

    if (inputType == "file") {
      for (_ <- Range(0, c.toString().toInt)) {
        arr.update(cursor, currentFileId)
        cursor += 1
      }
      currentFileId += 1
    } else {
      cursor += c.toString().toInt
    }
  }

  arr
}

private def diskSize(diskMap: String): Int = {
  diskMap
    .map(_.toString().toInt)
    .sum
}

private def prettyPrintDisk(disk: Array[Int]): Unit = {
  println(disk.map(x => if (x == -1) then "." else x).map(_.toString).mkString)
}

private def compactDisk(disk: Array[Int]): Unit = {
  val advanceWrite = advanceTo(_ == -1, 1)
  val advanceRead = advanceTo(_ != -1, -1)

  var writeCursor = advanceWrite(disk, 0)
  var readCursor = advanceRead(disk, disk.length - 1)

  while (writeCursor < readCursor) {
    disk.update(writeCursor, disk(readCursor))
    disk.update(readCursor, -1)
    writeCursor = advanceWrite(disk, writeCursor + 1)
    readCursor = advanceRead(disk, readCursor - 1)
  }
}

@tailrec private def advanceTo(
    predicate: (value: Int) => Boolean,
    increment: Int
)(
    disk: Array[Int],
    start: Int
): Int = {
  if (start >= disk.length || start < 0) {
    return -1
  }

  if (predicate(disk(start))) {
    return start
  }

  return advanceTo(predicate, increment)(disk, start + increment)
}

private def computeChecksum(disk: Array[Int]): Long = {
  disk.zipWithIndex
    .map((e, i) => if (e != -1) then e.toLong * i else 0)
    .sum
}

private def compactDiskWholeFiles(disk: Array[Int]): Unit = {
  val advanceRead = advanceTo(_ != -1, -1)
  var readCursor = advanceRead(disk, disk.length - 1)
  var lastSeenFileId = disk.max + 1

  while (readCursor > 0) {
    // prettyPrintDisk(disk)

    val currentFileSize = fileSizeAt(disk, readCursor, -1)
    val emptySpot =
      findEmptySpace(disk, currentFileSize, 0, readCursor - currentFileSize)
    val fileId = disk(readCursor)
    if (lastSeenFileId > fileId) {

      if (emptySpot != -1 && lastSeenFileId > fileId) {
        for (i <- Range(0, currentFileSize)) {
          disk.update(emptySpot + i, fileId)
          disk.update(readCursor - i, -1)
        }
      }

      lastSeenFileId = fileId
    }
    readCursor = advanceRead(disk, readCursor - currentFileSize)
  }

}

private def fileSizeAt(disk: Array[Int], startPos: Int, increment: Int): Int = {
  val fileId = disk(startPos)
  var readCursor = startPos

  while (
    readCursor > -1 && readCursor < disk.length && disk(readCursor) == fileId
  ) {
    readCursor += increment
  }
  // println(
  //   s"File ${fileId} between [${startPos}, ${readCursor}] has length ${Math.abs(startPos - readCursor)}"
  // )

  return Math.abs(startPos - readCursor)
}

@tailrec private def findEmptySpace(
    disk: Array[Int],
    size: Int,
    start: Int,
    endPos: Int
): Int = {
  val advance = advanceTo(_ == -1, 1)
  val cursor = advance(disk, start)
  if (cursor >= endPos) {
    return -1
  }

  val freeSpaceSize = fileSizeAt(disk, cursor, 1)

  if (freeSpaceSize >= size) {
    return cursor
  }
  return findEmptySpace(disk, size, cursor + freeSpaceSize, endPos)
}
