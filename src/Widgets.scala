#!/usr/bin/env -S scala-cli -S 3

package name.kopec.adam

object Widgets {

  sealed trait Widget

  object Widget {
    case object SmallBox extends Widget

    case object BigBox extends Widget

    case class Icon(code: String) extends Widget
  }

}

//let's add the behaviour: we need the widgets to descibe themselves

object LeveragePatternMatching {

  import Widgets.Widget

  def describe(widget: Widget): String = widget match {
    case Widget.SmallBox   => "SmallBox"
    case Widget.BigBox     => "BigBox"
    case Widget.Icon(code) => s"icon-$code"
  }
}

object ImplementDirectly {
  sealed trait Widget {
    def describe: String
  }

  object Widget {
    case object SmallBox extends Widget {
      override def describe: String = "SmallBox"
    }

    case object BigBox extends Widget {
      override def describe: String = "BigBox"
    }

    case class Icon(code: String) extends Widget {
      override def describe: String = s"icon-$code"
    }
  }
}

object TypeClassesPerType {

  import Widgets.Widget

  trait Describable[W]:
    extension (w: W) def describe: String

  given Describable[Widget.BigBox.type] with
    extension (w: Widget.BigBox.type) def describe: String = "BigBox"

  given Describable[Widget.SmallBox.type] with
    extension (w: Widget.SmallBox.type) def describe: String = "SmallBox"

  given Describable[Widget.Icon] with
    extension (w: Widget.Icon) def describe: String = s"icon-${w.code}"

  def describe[W <: Widget: Describable](widget: W): String =
    widget.describe
}

object TypeClassesPerBehaviour {
  import Widgets.Widget

  trait Descriptor[-D]:
    def describeIt(d: D): String

  object ByNameDescriptor extends Descriptor[Widget] {
    override def describeIt(d: Widget): String =
      d.getClass.getSimpleName.dropRight(1) // ^^
  }

  trait DescribableDescriptorAware[D]: // ^^
    extension (d: D) def descriptor: Descriptor[D]

  given DescribableDescriptorAware[Widget] with
    extension (d: Widget) def descriptor: Descriptor[Widget] = ByNameDescriptor

  given DescribableDescriptorAware[Widget.Icon] with
    extension (w: Widget.Icon)
      def descriptor: Descriptor[Widget.Icon] = (i: Widget.Icon) =>
        s"icon-${i.code}"

  trait Describable[W]:
    extension (w: W) def describe: String

  given defDescribable[W](using
      desc: DescribableDescriptorAware[W]
  ): Describable[W] with
    extension (w: W) def describe: String = desc.descriptor(w).describeIt(w)

  def describe[W <: Widget: Describable](widget: W): String =
    widget.describe
}

@main def main: Unit = {
  import Widgets.Widget
  import Widgets.Widget.*
  import LeveragePatternMatching.*
  // import TypeClassesPerBehaviour.{given, *}
  // import TypeClassesPerType.{given, *}
  println(describe(Icon("star")))
}
