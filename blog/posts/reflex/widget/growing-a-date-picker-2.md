---
title: Growing a Date Picker in Reflex - Part 2
date: 2017-10-18
authors: schalmers
project: reflex
extra-css: /css/reflex/growing-a-datepicker/reflex-dom-datepicker.css, /css/reflex/growing-a-datepicker/part2.css
extra-js: /js/reflex/growing-a-datepicker/datepicker-embed.min.js
---

Continuing on from the [previous post](posts/reflex/widget/growing-a-datepicker-1). A lot has changed and the various pieces are really starting to come together. In this post we discuss a large scale refactoring and some redesign to facilitate more flexible and useful styling.

### Behold, a datepicker

The widget on the left is the simple date picker implementation as it currently stands. The CSS that is provided in the library itself is far simpler for portability reasons. Also because my skills with CSS are quite woeful, I do apologise.

<div id="datepicker-simple"></div>

### Refactoring

Spurred on by [this comment](https://www.reddit.com/r/haskell/comments/74mnnk/growing_a_date_picker_in_reflex_part_1/do3g6nx/), I started to factor out the larger pieces of the date picker into their own functions, and eventually their own modules. Initially this was to see if I could make the list of days a standalone item, but it soon snowballed...

#### Modules!

There are now three primary components that are available for individual use:

- ``Reflex.Dom.Widget.Input.Datepicker.Controls``: This is the 'Previous' and 'Next' month buttons, along with the text input. Providing the ``Event``s for the clicks and text input.
```haskell
mkDatePickerControls
  :: MonadWidget t m
  => DateInputConfig t
  -> Wrap ControlW t m
  -> Wrap MonthBtnW t m
  -> Event t Day
  -> m (DatePickerControls t)
mkDatePickerControls = ...
```

- ``Reflex.Dom.Widget.Input.Datepicker.DaySelect``: This is the widget that will display a list of days and provide an ``Event`` when one is clicked.
```haskell
mkDaySelectDisplay
  :: MonadWidget t m
  => TimeLocale
  -> DayFormat
  -> Wrap DayW t m
  -> Wrap DayListW t m
  -> Dynamic t Day
  -> Dynamic t [Day]
  -> Dynamic t (Bool -> Map Text Text)
  -> m (Event t Day)
mkDaySelectDisplay = ...
```

- ``Reflex.Dom.Widget.Input.Datepicker.Core``: This is the core functionality of the date picker widget. Providing two ``Dynamic``s for the ``Day`` and ``[Day]`` based on the current inputs. As well as taking care of the parsing of the ``Text`` input based on the given format.
```haskell
mkDatePickerCore
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     )
  => DatePickerCore t
  -> m (Dynamic t Day, Dynamic t [Day])
mkDatePickerCore = ...
```

Each of these components can be used as a standalone piece. So if you have an existing way of specifying a list of ``Day`` values, you can use only the ``DaySelect`` component to manage the list and the click ``Event``. If you have your own display built, you can use the ``Core`` component to manage the state of the ``Day`` value. This provides a lot of flexibility, in display, since none of the components are bound together, but also in raw functionality with respect to how you integrate the various ``Event``s and ``Dynamic``s.

Naturally everyone will have their own requirements, so I'm quite interested in the feedback regarding this sort of design and if it helps provide the desired level of flexibility.

There is also a simpler datepicker function included that packages everything up into a single widget. Should you have no need to utilise the pieces individually. 

There is also:

- ``Reflex.Dom.Widget.Input.Datepicker.Style``: That contains some ``Map Text Text`` implementations for element attributes, and some ``Wrap a t m`` defaults.

- ``Reflex.Dom.Widget.Input.Datepicker.Types``: Reasonably self-explanatory, it contains the core records that hold various sets of information, as well as some utility functions. As well as the ``Wrap a t m`` and the type level identifiers for the datepicker wrappers.

- ``Reflex.Dom.Widget.Basic.SelectViews``: Contains some generalised 'list of widgets' type functions that are variations on functions provided in ``reflex-dom``.

### Paint and glitter

The ability to dynamically style the ``Day`` that has been selected is a required feature. There are quite a few ways of handling this and I went through a few iterations before settling on what is there now. Even so, I'm sure there are better ways to do this.

Initial thoughts involved changing the attributes ``Dynamic`` from:
```haskell
Dynamic t (Map Text Text)
```
To something like:
```haskell
Dynamic t (Day -> Day -> Map Text Text)
```
This in turn could be combined with a function:
```haskell
Map Text Text -> Day -> Day -> Map Text Text
```

That would let you make adjustments to the default styling for the ``Day`` based on, for example, comparison to another ``Day`` value. So if the ``Day`` values match, in the case of trying to render the ``Day`` that has been clicked, you can adjust the styling to suit. The construction of that function would be left to the end user, with some default implementations provided for people that don't care to mess with it.

This ended up being quite clumsy to use, and gave the nagging feeling that there would be endless edge-cases where this function was either not flexible enough, or there would always be redundant inputs. So I moved on...

After reading more into the existing functionality that is available in ``reflex-dom``, one function in particular stood out:
```haskell
selectViewListWithKey_
  :: ( ... )
  => Dynamic t k
  -> Dynamic t (Map k v)
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a))
  -> m (Event t k)
```
This will use the given function to display each ``v`` and provide a ``Dynamic t Bool`` that indicates if the current value of ``Dynamic t k`` is the ``k`` for the ``Dynamic t v`` that is being displayed.

I attempted to utilise the following structure:
```haskell
Dynamic t (Map Day (Bool -> Map Text Text))
```
Because that would have given:
```haskell
_ :: Day -> Dynamic t (Bool -> Map Text Text) -> Dynamic t Bool -> m (Event t Day)
```
Allowing me to use the ``Applicative`` instance for ``Dynamic`` to combine the ``Dynamic t Bool`` and the ``Dynamic t (Bool -> Map Text Text)``:
```haskell
let dAttrs = dAttrFn <*> dSelected
```
Creating the ``Dynamic t (Map Text Text)`` that is needed for the inner element.

However this ended up being more trouble than it was worth. I had to do a few backflips to wire together other components, and it involved duplicating the ``Map Text Text`` for every day of the given month. The Haskell and GHCJS runtime is very (**very**) clever, and would likely notice the duplication and optimise accordingly. But it still didn't seem like a wise design choice. Given that every ``Day`` item has identical attributes, save for the selected value, I thought I could do better.

The result was the following function:
```haskell
selectViewListDynAttr
  :: forall t m v a.
     ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Eq v
     , Ord v
     )
  => Dynamic t v
  -> Dynamic t [v]
  -> Dynamic t (Bool -> Map Text Text)
  -> (Dynamic t v -> Dynamic t (Map Text Text) -> m (Event t a))
  -> m (Event t a)
```
This is a rough duplication of the ``selectViewListWithKey_`` function, with some changes to accommodate the ``Dynamic t (Bool -> Map Text Text)``. Internally it uses a function that still requires a ``Dynamic t (Map k v)``. But without needing to duplicate the ``(Bool -> Map Text Text)`` for every ``Day`` of the given month.

With this written, the only real change was including a ``Dynamic t Day`` for the currently selected value as an input to the ``mkDaySelectDisplay`` function. Then changing the default attributes for a ``Day`` item from:
```haskell
dayElAttrs :: Reflex t => Dynamic t (Map Text Text)
dayElAttrs = pure ("class" =: "day-item")
```
To:
```haskell
dayElAttrs :: Reflex t => Dynamic t (Bool -> Map Text Text)
dayElAttrs = pure (\selected ->
                     "class" =: ( "day-item" <> if selected then " active" else "" )
                  )
```

Giving us a ``Dynamic t (Map Text Text)`` which takes into account if this is the ``Day`` that has been selected. We also don't have to do the plumbing for what happens when a different day is selected. We also don't have to create a clunky map with explicitly duplicated attributes for every day in the month.

I'm still not completely sure that this is the best signature for the ``Dynamic`` function, given that you can't style the item based on the value of item itself. So if you want to style a public holiday differently, this doesn't directly allow it. But I'm wary of expanding this function too much.

The functions it builds on are in the [reflex-dom-datepicker](https://github.com/qfpl/reflex-dom-datepicker) repo, and if they're useful enough then they might end up in ``reflex-dom``. 

### Scope creep...

It would be nice to be able to select a range of days. Either by selecting a start and end individually, or clicking and dragging. That's a bit more advanced and I'm not sure how to approach it yet.

There is no functionality for hiding or showing the list of days in a pop-over, when the text input has focus. Which is a common DatePicker 'feature'. The ``DatePickerControls`` record does contain the ``TextInput`` record, which has a ``Dynamic t Bool`` for when the text input has focus, with access to all of the styling it should be reasonably mechanical to implement. It'd be the positioning CSS that would be a pain, I expect.

### Styling additions

In order to make some of the styling possible for the various component layouts, I needed to wrap groups of elements in a ``div`` or similar parent element. Whilst "make it work" was the priority, this was handled by hard-coding the wrapping element. But that isn't a viable solution for end users as their needs will always be a bit different. It is easy to assume they'll be more capable than I am when it comes to CSS, so best not bind them to my attempts at a flexible layout.

To that end, I generalised the technique I had used in earlier stages by having a ``newtype`` that contained a wrapping function:
```haskell
newtype Wrap a t m = Wrap
  { wrapEl :: forall e. MonadWidget t m => m e -> m e
  }
```
This type contains a [phantom type](https://wiki.haskell.org/Phantom_type) which requires you to provide extra type level information. In this case, the extra information is an identifier about the element you will be wrapping. As well as ensuring that the ``t`` and ``m`` line up with the rest of the Reflex application. It is constructed using a function that is the identity of the element that is passed in, whilst allowing you to wrap the inner element in something else.

To use it, specify an empty type, so we have a type level identifier about what you're intending to use it for:
```haskell
data ControlWrap
```
This is a void type as there are no constructors, it's a type level restriction to ensure we don't try to wrap things with the wrong wrapper.

For our example we'll use a ``div`` with a class of our choosing:
```haskell
import Reflex.Dom (MonadWidget, divClass)

controlWrap :: MonadWidget t m => Wrap ControlWrap t m
controlWrap = Wrap (divClass "my-controls")
```

Then, when you want to wrap the element:
```haskell
myEl :: MonadWidget t m => m (Event t ())
myEl = ...

wrappedEl :: MonadWidget t m => Wrap ControlWrap t m -> m (Event t ())
wrappedEl myWrap = wrapEl myWrap $ myEl
```

This lets you specify specific wrappers for given elements:
```haskell
-- Separate the wrappers for different elements clearly
dayList :: MonadWidget t m => ... -> Wrap DayWrap t m -> Wrap DayListWrap t m -> ...
mkDatePickerControls :: MonadWidget t m => ... -> Wrap ControlWrap t m -> ...
```
Allowing the type system to help you use the correct wrapper.

This is something that is used in this project, and it's trivial enough that you can make your own should you feel so inclined. But there are possibilities for creating higher order functions with this style that would allow for safer definitions of frameworks like [Bootstrap](https://getbootstrap.com/). Stating clearly in the type that a function named ``cFluid`` has a type of ``:: Wrap ContainerFluid t m`` and you don't have to rely on the name only to use the right wrapper in the right spot.

There are possibly type level functions one could implement that would make that even nicer, but that is beyond the scope of this post.

Additionally, there is a lot of work to be done to make a nicer, composible, and type safe method of defining and using attributes/CSS. But that is a huuuuuuuuge project.

### Oh no, a bug!

Overall the process of breaking out the different pieces into their various modules was quite painless, the type system told me about the things I'd forgotten, and the semantics of FRP and how the ``Event``s and ``Dynamic``s fit together ensured that nothing untoward happened. Mostly...

During my search for a cleaner way to express the construction of an ``Event t Day`` from the ``Dynamic t Day`` and ``Event t ()``, I completely broke my widget... What I had was:
```haskell
let ePreviousMonth = prevMonth <$> current dDayValue <@ ePrevMonthClicked
```
This creates an ``Event`` that contains a value of the previous month, based on the current value of the ``Dynamic``, tagged at the time of the ``Event t ()`` from the previous month button being clicked.

This is a common enough expression in Reflex, however I wondered if there was a simpler way to express this. What I found was:
```haskell
tagPromptlyDyn :: Dynamic a -> Event b -> Event a

-- compared to the '<@' operator, aka tag, and 'current'
current :: Dynamic a -> Behavior a
(<@) :: Behavior a -> Event b -> Event a
```
So my expression became:
```haskell
let ePreviousMonth = prevMonth <$> tagPromptlyDyn dDayValue ePrevMonthClicked
```
Saved the file, compiled the code, reloaded the page, and nothing worked. Bugger...

The issue is that whilst ``tagPromptlyDyn d e`` resembles ``tag (current d) e``, it has different functionality with respect to acquiring the value of ``d``. Explained in detail [here](https://github.com/reflex-frp/reflex/blob/fe21a501f7db4a3dbf5f7727c37bbe33fafee9ac/src/Reflex/Dynamic.hs#L242). I had created the exact scenario that the comment is referring to. As this particular ``Event`` is used to update the value of the ``Dynamic`` that I am using the ``current`` value of, in order to compute the new value. This creates a dependency cycle, thus the widget did not function.

Changing it back to use ``current d <@ e`` made everything happy. ``tagPromptlyDyn`` does have its uses, but in this case the functionality was incorrect for what I was trying to achieve.

### Just keep swimming...

The date picker is progressing well and it's nice to see people interested in it. Next goal is proper testing, for both the widget side of things, and the underlying FRP implementation. The compositional design and robustness of FRP is excellent and provides an immense amount of confidence. But it is not the full story and I would like to know how to write tests for a Reflex widget, and Reflex in general. Also, I **must** write some documentation about how to use this widget.
