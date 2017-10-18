---
title: Growing a Date Picker in Reflex - Part 2
date: 2017-10-18
authors: schalmers
project: reflex
extra-css: /css/reflex/growing-a-datepicker/reflex-dom-datepicker.css, /css/reflex/growing-a-datepicker/part2.css
extra-js: /js/reflex/growing-a-datepicker/datepicker-embed.min.js
---

Continuing on from the [previous post](posts/reflex/widget/growing-a-datepicker-1) about our shiny new date picker, there has been a lot going on and the various pieces are really starting to come together. In this post we discuss a large scale refactoring of the larger moving parts into self-contained pieces.

#### The story so far

<div id="datepicker-simple"></div>
<small>
As it turns out, I'm worse with CSS than I thought. The research continues...
</small>

#### Refactoring

Spurred on by [this comment](https://www.reddit.com/r/haskell/comments/74mnnk/growing_a_date_picker_in_reflex_part_1/do3g6nx/), I started to factor out the larger pieces of the date picker into their own functions, and eventually their own modules. Initially this was to see if I could make the 'day' display widget a standalone item. This soon snowballed however.

#### Modules!

There are now three primary components that are available for individual use:

- ``Controls``: This is the 'Previous' and 'Next' month buttons, along with the text input. Providing the ``Event``s for the clicks and text input.
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

- ``DaySelect``: This is the widget that will display a list of days and provide an ``Event`` when one is clicked.
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

- ``Core``: This is the core functionality of the date picker widget. Providing two ``Dynamic``s for the ``Day`` and ``[Day]`` based on the current inputs. As well as taking care of the parsing of the ``Text`` input based on the given format.
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

The ability to style the day that has been selected is a required feature. There quite a few ways of handling this requirement and I went through quite a few iterations before settling on this particular one. Even now I'm sure there are better ways to do this, but this will suffice for now.

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

This ended up being quite clumsy to use and gave the nagging feeling that there would be endless edge cases where this function was either not flexible enough, or there would always be redundant inputs. So I moved on...

After reading more into the existing functionality in ``reflex-dom``, one function in particular stood out:
```haskell
selectViewListWithKey_
  :: ( ... )
  => Dynamic t k
  -> Dynamic t (Map k v)
  -> (k -> Dynamic t v -> Dynamic t Bool -> m (Event t a))
  -> m (Event t k)
```
Which will use the given function to display each ``v`` and provide a ``Dynamic t Bool`` that indicates if key for this value matches the value in the provided ``Dynamic t k``.

I attempted to utilise the following structure:
```haskell
Dynamic t (Map Day (Bool -> Map Text Text))
```
Because that would have allowed me to create a function:
```haskell
_ :: Day -> Dynamic t (Bool -> Map Text Text) -> Dynamic t Bool -> m (Event t Day)
```

However this ended up being more trouble than it was worth, and would have involved duplicating the attribute map for every day of the given month. The Haskell and GHCJS runtime is very (**very**) clever, but this still didn't seem like a wise decision. Given that every ``Day`` item has identical attributes, save for the selected value, I thought I could do better.

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
This allows us, leaning on the ``Applicative`` instance for ``Dynamic`` to combine it with the ``Dynamic t Bool``:
```haskell
dAttributeFn <*> dSelected
```
Giving us a ``Dynamic t (Map Text Text)`` which takes into account if this is the ``Day`` that has been selected. We also don't have to do the plumbing for what happens when a different day is selected. We also don't have to create a clunky map with explicitly duplicated attributes for every day in the month.

I'm still not completely sure that this is the best signature for the ``Dynamic`` function, given that you can't style the item based on the value of item itself. So if you want to style a public holiday differently, this doesn't directly allow it. But I'm wary of expanding this function too much.

The functions it builds on are in the [reflex-dom-datepicker](https://github.com/qfpl/reflex-dom-datepicker) repo, and if they're useful enough then they might end up in ``reflex-dom``. 

#### Scope creep...

There is also the need to be able to select a range of days. Either by clicking start and end days, or clicking and dragging. That's a bit more advanced and I'm not sure how to approach that one yet.

### Styling additions

In order to make some of the styling possible for the various component layouts, there is a need to wrap groups of elements in a ``div`` or similar parent element. Whilst "make it work" was the priority, this was handled by hard-coding in the wrapping element. But that isn't a viable solution for end users as their needs will always be a bit different. Easy to assume they'll be better with CSS too, so best not bind them to my attempts at a flexible layout.

To that end, I generalised the technique I had used in earlier stages by having a ``newtype`` that contained a wrapping function:
```haskell
newtype Wrap a t m = Wrap
  { wrapEl :: forall e. MonadWidget t m => m e -> m e
  }
```
This type contains a ``Phantom Type`` that let you specify type level information about what sort of element you will be wrapping. As well as ensuring that the ``t`` and ``m`` line up. It is constructed using a function that is effectively the identity of the element that is passed in. But gives a chance to wrap the inner element in something else.

To use it, specify a type to provide information about what you're intending to use it for:
```haskell
data ControlW
```
This is a void type as the value is not relevant, it's a type level restriction to ensure we don't try to wrap things with the wrong wrapper.

For our example we'll just use a ``div`` with a class of our choosing:
```haskell
import Reflex.Dom (MonadWidget, divClass)

controlWrap :: MonadWidget t m => Wrap ControlW t m
controlWrap = Wrap (divClass "my-controls")
```

Then, when you want to wrap the element:
```haskell
myEl :: MonadWidget t m => m (Event t ())
myEl = ...

wrappedEl :: MonadWidget t m => Wrap ControlW t m -> m (Event t ())
wrappedEl myWrap = wrapEl myWrap $ myEl
```

This lets you specify specific wrappers for given elements:
```haskell
-- Separate the wrappers for different elements clearly
dayList :: MonadWidget t m => ... -> Wrap DayW t m -> Wrap DayListW t m -> ...
mkDatePickerControls :: MonadWidget t m => ... -> Wrap ControlW t m -> ...
```
Allowing the type system to help you use the correct wrapper.

This is something that is just used in this project, and it's trivial enough that you can make your own should you feel so inclined. But there are possibilities for creating higher order functions with this style that would allow for safer definitions of frameworks like [Bootstrap](https://getbootstrap.com/). Stating clearly in the type that a function named ``cFluid`` has a type of ``:: Wrap ContainerFluid t m`` and you don't have to rely on the name only to use the right wrapper in the right spot.

There are type level functions that would make that even nicer but that is beyond the scope of this post.

#### Pretty!

There is now some CSS included with the date picker and the layout of the controls and the list of days in the month now looks like something worthwhile. It's a work in progress and there many yaks lining up for this particular barber. So it most cases you will be better off writing your own styles to apply to the date picker, until I come up with something more robust.

### Oh no, a bug!

Overall the process of breaking out the different pieces into their various modules was quite painless, the type system told me about the things I'd forgotten, and the semantics of FRP and how the ``Event``s and ``Dynamic``s fit together ensured that nothing untoward happened. Mostly...

One issue I encountered, and it was entirely my fault, was wanting to find a cleaner way to express the construction of an ``Event t Day`` from the ``Dynamic t Day`` and ``Event t ()``. What I had was:
```haskell
let ePreviousMonth = prevMonth <$> current dDayValue <@ ePrevMonthClicked
```
Which creates an ``Event`` that contains a value of the previous month, based on the current value of the ``Dynamic``, tagged at the time of the ``Event t ()`` from the previous month button being clicked.

This is a common enough expression in FRP, however I wondered if there was a simpler way to express this, given how common it seemed to me. What I found was a function:
```haskell
tagPromptlyDyn :: Dynamic a -> Event b -> Event a
-- Vs the 'tag' operator and 'current'
current :: Dynamic a -> Behavior a
(<@) :: Behavior a -> Event b -> Event a
```
So my expression became:
```haskell
let ePreviousMonth = prevMonth <$> tagPromptlyDyn dDayValue ePrevMonthClicked
```
Saved the file, compiled the code, reloaded the page, and nothing worked. Damn...

The issue is that whilst ``tagPromptlyDyn d e`` resembles ``tag (current d) e``, it has different functionality with respect to acquiring the value of ``d``. Explained in detail [here](https://github.com/reflex-frp/reflex/blob/fe21a501f7db4a3dbf5f7727c37bbe33fafee9ac/src/Reflex/Dynamic.hs#L242). I had created the exact scenario that the comment is referring to. As this particular ``Event`` is used to update the value of the ``Dynamic`` that I am using the ``current`` value of, in order to compute the new value. This creates a dependency cycle, thus the widget did not function.

Changing it back to use ``current d <@ e`` made everything happy. ``tagPromptlyDyn`` does have its uses, but in this case the functionality was incorrect for what I was trying to achieve.

## Just keep swimming...

The date picker is progressing well and it's nice to see people interested in it. Next goal is proper testing, for both the widget side of things, and the underlying FRP implementation. The compositional design and robustness of FRP is excellent and provides an immense amount of confidence. But it is not the full story and I would like to know how to write tests for a Reflex widget, and Reflex in general. Also, I **must** write some documentation.
