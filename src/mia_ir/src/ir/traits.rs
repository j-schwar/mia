use id_arena::{Arena, DefaultArenaBehavior, Id, Iter};

/// Trait interface for an id arena.
pub trait IdArena<I> {
	/// Exposes a reference to this type's arena.
	fn arena(&self) -> &Arena<I>;

	/// Exposes a mutable reference to this type's arena.
	fn arena_mut(&mut self) -> &mut Arena<I>;

	/// Allocates a new item in this arena returning a unique identifier which
	/// can be used to retrieve it.
	fn alloc(&mut self, item: I) -> Id<I> {
		self.arena_mut().alloc(item)
	}

	/// Allocates a new item in this arena returning a unique identifier which
	/// can be used to retrieve it.
	///
	/// This method allows for the creation of values which hold their id
	/// internally within themselves.
	fn alloc_with_id<F>(&mut self, f: F) -> Id<I>
	where
		F: FnOnce(Id<I>) -> I,
	{
		self.arena_mut().alloc_with_id(f)
	}

	/// Returns the item associated with `id`. If no such item exists in this
	/// arena, then `None` is returned.
	fn get(&self, id: Id<I>) -> Option<&I> {
		self.arena().get(id)
	}

	/// Returns the item associated with `id`. If no such item exists in this
	/// arena, then `None` is returned.
	fn get_mut(&mut self, id: Id<I>) -> Option<&mut I> {
		self.arena_mut().get_mut(id)
	}

	/// An iterator over the values in this arena.
	fn iter(&self) -> Iter<I, DefaultArenaBehavior<I>> {
		self.arena().iter()
	}

	/// Returns the item associated with `id` panicking if unable to find such
	/// an item in this arena.
	///
	/// For a non-panicking version of this method, see [`get`].
	///
	/// [`get`]: ./trait.IdArena.html#method.get
	///
	/// ## Panics
	///
	/// This method panics if no item with the given id can be found in this
	/// arena.
	fn get_unwrap(&self, id: Id<I>) -> &I {
		self.get(id)
			.expect(&format!("unable to find item with id: {:?}", id))
	}

	/// Returns the item associated with `id` panicking if unable to find such
	/// an item in this arena.
	///
	/// For a non-panicking version of this method, see [`get_mut`].
	///
	/// [`get_mut`]: ./trait.IdArena.html#method.get
	///
	/// ## Panics
	///
	/// This method panics if no item with the given id can be found in this
	/// arena.
	fn get_mut_unwrap(&mut self, id: Id<I>) -> &mut I {
		self.get_mut(id)
			.expect(&format!("unable to find item with id: {:?}", id))
	}

	/// Returns the id and reference of the first item in this arena which
	/// satisfies `predicate`.
	fn find<P>(&self, predicate: P) -> Option<(Id<I>, &I)>
	where
		P: Fn(&I) -> bool,
	{
		self.iter().find(|(_, item)| predicate(item))
	}

	/// Returns the id and reference of the first item in this arena which
	/// has a given name.
	fn find_with_name(&self, name: &str) -> Option<(Id<I>, &I)>
	where
		I: MaybeNamed,
	{
		self.find(|i| i.name() == Some(name))
	}
}

/// Trait for objects which may contain a textual name.
///
/// Types which implement this trait are searchable in arenas by their name.
pub trait MaybeNamed {
	/// The name of the object if it has one.
	fn name(&self) -> Option<&str>;
}
