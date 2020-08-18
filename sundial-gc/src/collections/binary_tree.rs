use self::sundial_gc::*;
use crate as sundial_gc;
use smallvec::SmallVec;
use std::{cmp::Ordering::*, fmt::Debug, iter::FromIterator, ops::Index};
use sundial_gc_derive::*;

#[derive(Trace, Ord, PartialOrd, Eq, PartialEq)]
pub struct Map<'r, K, V>(Option<Gc<'r, Node<'r, K, V>>>)
where
    K: 'r,
    V: 'r;

impl<'r, K, V> Copy for Map<'r, K, V> {}
impl<'r, K, V> Clone for Map<'r, K, V> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'r, K, V> Default for Map<'r, K, V> {
    #[inline(always)]
    fn default() -> Self {
        Map(None)
    }
}

impl<'r, K, V> From<Gc<'r, Node<'r, K, V>>> for Map<'r, K, V> {
    #[inline(always)]
    fn from(node: Gc<'r, Node<'r, K, V>>) -> Self {
        Map(Some(node))
    }
}

impl<'r, K: Trace + Clone + Ord, V: Trace + Clone> FromIterator<(K, V)> for Map<'r, K, V> {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let arena = &Arena::new();
        iter.into_iter()
            .fold(Map::default(), |map, (k, v)| map.insert(k, v, arena))
    }
}

impl<'r, K: Trace + Ord + Clone + Debug, V: Trace + Clone + Debug> Debug for Map<'r, K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<'r, K: Trace + Ord + Clone, V: Trace + Clone> Index<&K> for Map<'r, K, V> {
    type Output = V;
    fn index(&self, key: &K) -> &V {
        self.get(key).unwrap()
    }
}

impl<'r, K: Trace + Ord + Clone, V: Trace + Clone> Map<'r, K, V> {
    pub fn size(&self) -> usize {
        match self {
            Map(Some(n)) => n.size,
            Map(None) => 0,
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        match &self.0 {
            Some(n) => n.get(key),
            None => None,
        }
    }

    pub fn insert(self, key: K, value: V, arena: &Arena<Node<'r, K, V>>) -> Map<'r, K, V> {
        match self.0 {
            Some(n) => Map(Some(Node::insert(n, key, value, arena))),
            _ => Map::from(arena.gc(Node {
                key,
                value,
                left: Map::default(),
                right: Map::default(),
                size: 1,
            })),
        }
    }

    pub fn insert_if_empty(self, key: K, value: V, arena: &Arena<Node<'r, K, V>>) -> Map<'r, K, V> {
        match self.0 {
            Some(n) => Map(Some(Node::insert(n, key, value, arena))),
            _ => Map::from(arena.gc(Node {
                key,
                value,
                left: Map::default(),
                right: Map::default(),
                size: 1,
            })),
        }
    }

    pub fn iter(self) -> Iter<'r, K, V> {
        let mut parents = SmallVec::new();

        if let Map(Some(mut node)) = self {
            parents.push(node);

            while let Map(Some(left)) = node.left {
                node = left;
                parents.push(left);
            }

            debug_assert!(node.left.0.is_none());
        };

        Iter { parents }
    }

    /// TODO non naive implementation.
    pub fn union(self, right: Map<'r, K, V>, arena: &Arena<Node<'r, K, V>>) -> Map<'r, K, V> {
        match (self, right) {
            (Map(Some(left @ Gc(_, _))), Map(Some(Gc(_, _)))) => {
                let left: Gc<Node<K, V>> = left;
                let node = right.iter().fold(left, |left, (k, v)| {
                    Node::insert_if_empty(left, k.clone(), v.clone(), arena)
                });
                Map::from(node)
            }
            (Map(Some(_)), _) => self,
            (_, Map(Some(_))) => right,
            _ => Map(None),
        }
    }
}

#[derive(Trace, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Node<'r, K, V>
where
    K: 'r,
    V: 'r,
{
    key: K,
    size: usize,
    left: Map<'r, K, V>,
    right: Map<'r, K, V>,
    value: V,
}

impl<'r, K: Trace + Debug + Clone + Ord, V: Trace + Debug + Clone> Debug for Node<'r, K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node")
            .field("key", &self.key)
            .field("value", &self.value)
            .field("size", &self.size)
            .field("left", &self.left)
            .field("right", &self.right)
            .finish()
    }
}

impl<'r, K: Trace + Ord + Clone, V: Trace + Clone> Node<'r, K, V> {
    pub fn get(&self, key: &K) -> Option<&V> {
        match (key.cmp(&self.key), &self.left, &self.right) {
            (Equal, _, _) => Some(&self.value),
            (Less, Map(Some(node)), _) | (Greater, _, Map(Some(node))) => node.get(key),
            _ => None,
        }
    }

    pub fn insert(
        node: Gc<'r, Node<'r, K, V>>,
        key: K,
        value: V,
        arena: &Arena<Node<'r, K, V>>,
    ) -> Gc<'r, Node<'r, K, V>> {
        match key.cmp(&node.key) {
            Equal => arena.gc(Node {
                key,
                value,
                left: node.left,
                right: node.right,
                size: node.size,
            }),
            Less => Node::balance(
                node.key.clone(),
                node.value.clone(),
                node.left.insert(key, value, arena),
                node.right,
                arena,
            ),
            Greater => Node::balance(
                node.key.clone(),
                node.value.clone(),
                node.left,
                node.right.insert(key, value, arena),
                arena,
            ),
        }
    }

    pub fn insert_if_empty(
        node: Gc<'r, Node<'r, K, V>>,
        key: K,
        value: V,
        arena: &Arena<Node<'r, K, V>>,
    ) -> Gc<'r, Node<'r, K, V>> {
        match key.cmp(&node.key) {
            Equal => node,
            Less => Node::balance(
                node.key.clone(),
                node.value.clone(),
                node.left.insert(key, value, arena),
                node.right,
                arena,
            ),
            Greater => Node::balance(
                node.key.clone(),
                node.value.clone(),
                node.left,
                node.right.insert(key, value, arena),
                arena,
            ),
        }
    }

    const DELTA: usize = 3;
    const RATIO: usize = 2;

    fn balance(
        key: K,
        value: V,
        left: Map<'r, K, V>,
        right: Map<'r, K, V>,
        arena: &Arena<Self>,
    ) -> Gc<'r, Self> {
        let node = |key: K, value: V, size, left, right| {
            arena.gc(Node {
                key,
                value,
                size,
                left,
                right,
            })
        };

        match left {
            Map(None) => match right {
                Map(None) => node(key, value, 1, Map::default(), Map::default()),

                Map(Some(Gc(
                    Node {
                        left: Map(None),
                        right: Map(None),
                        ..
                    },
                    _,
                ))) => node(key, value, 2, Map::default(), right),

                Map(Some(Gc(
                    Node {
                        key: rk,
                        value: rv,
                        left: Map(None),
                        right: rr @ Map(Some(_)),
                        ..
                    },
                    _,
                ))) => node(
                    rk.clone(),
                    rv.clone(),
                    3,
                    Map::from(node(key, value, 1, Map::default(), Map::default())),
                    *rr,
                ),

                Map(Some(Gc(
                    Node {
                        key: rk,
                        value: rv,
                        left:
                            Map(Some(Gc(
                                Node {
                                    key: rlk,
                                    value: rlv,
                                    ..
                                },
                                _,
                            ))),
                        right: Map(None),
                        ..
                    },
                    _,
                ))) => node(
                    rlk.clone(),
                    rlv.clone(),
                    3,
                    Map::from(node(key, value, 1, Map::default(), Map::default())),
                    Map::from(node(
                        rk.clone(),
                        rv.clone(),
                        1,
                        Map::default(),
                        Map::default(),
                    )),
                ),

                Map(Some(Gc(
                    Node {
                        key: rk,
                        value: rv,
                        size: rs,
                        left:
                            rl
                            @
                            Map(Some(Gc(
                                Node {
                                    key: rlk,
                                    value: rlv,
                                    size: rls,
                                    left: rll,
                                    right: rlr,
                                },
                                _,
                            ))),
                        right: rr @ Map(Some(Gc(Node { size: rrs, .. }, _))),
                        ..
                    },
                    _,
                ))) => {
                    if *rls < Self::RATIO * rrs {
                        node(
                            rk.clone(),
                            rv.clone(),
                            rs + 1,
                            Map::from(node(key, value, rls + 1, Map::default(), *rl)),
                            *rr,
                        )
                    } else {
                        node(
                            rlk.clone(),
                            rlv.clone(),
                            rs + 1,
                            Map::from(node(key, value, 1 + rll.size(), Map::default(), *rll)),
                            Map::from(node(
                                rk.clone(),
                                rv.clone(),
                                rrs + rlr.size() + 1,
                                *rlr,
                                *rr,
                            )),
                        )
                    }
                }
            },

            // left is Some
            Map(Some(Gc(
                Node {
                    key: lk,
                    value: lv,
                    size: ls,
                    left: ll,
                    right: lr,
                },
                _,
            ))) => match right {
                Map(None) => match (ll, lr) {
                    (Map(None), Map(None)) => arena.gc(Node {
                        key,
                        value,
                        size: 2,
                        left,
                        right: Map::default(),
                    }),

                    (
                        Map(None),
                        Map(Some(Gc(
                            Node {
                                key: lrk,
                                value: lrv,
                                ..
                            },
                            _,
                        ))),
                    ) => node(
                        lrk.clone(),
                        lrv.clone(),
                        3,
                        Map::from(node(
                            lk.clone(),
                            lv.clone(),
                            1,
                            Map::default(),
                            Map::default(),
                        )),
                        Map::from(node(key, value, 1, Map::default(), Map::default())),
                    ),

                    (Map(Some(_)), Map(None)) => node(
                        lk.clone(),
                        lv.clone(),
                        3,
                        *ll,
                        Map::from(node(key, value, 1, Map::default(), Map::default())),
                    ),

                    (
                        Map(Some(Gc(Node { size: lls, .. }, _))),
                        Map(Some(Gc(
                            Node {
                                key: lrk,
                                value: lrv,
                                size: lrs,
                                left: lrl,
                                right: lrr,
                            },
                            _,
                        ))),
                    ) => {
                        if *lrs < Self::RATIO * lls {
                            node(
                                lk.clone(),
                                lv.clone(),
                                ls + 1,
                                *ll,
                                Map::from(node(key, value, 1, Map::default(), Map::default())),
                            )
                        } else {
                            node(
                                lrk.clone(),
                                lrv.clone(),
                                ls + 1,
                                Map::from(node(
                                    lk.clone(),
                                    lv.clone(),
                                    lls + lrl.size() + 1,
                                    *ll,
                                    *lrl,
                                )),
                                Map::from(node(key, value, lrr.size() + 1, *lrr, Map::default())),
                            )
                        }
                    }
                },

                // left and right are Some
                Map(Some(Gc(
                    Node {
                        key: rk,
                        value: rv,
                        size: rs,
                        left: rl,
                        right: rr,
                    },
                    _,
                ))) => {
                    if *rs > Self::DELTA * ls {
                        let Node {
                            key: rlk,
                            value: rlv,
                            size: rls,
                            left: rll,
                            right: rlr,
                        } = rl.0.unwrap().0;
                        let Node { size: rrs, .. } = rr.0.unwrap().0;

                        if *rls < Self::RATIO * rrs {
                            node(
                                rk.clone(),
                                rv.clone(),
                                ls + rs + 1,
                                Map::from(node(key, value, ls + rll.size() + 1, left, *rl)),
                                Map::from(*rr),
                            )
                        } else {
                            node(
                                rlk.clone(),
                                rlv.clone(),
                                ls + rs + 1,
                                Map::from(node(key, value, ls + rll.size() + 1, left, *rll)),
                                Map::from(node(
                                    rk.clone(),
                                    rv.clone(),
                                    rrs + rlr.size() + 1,
                                    *rlr,
                                    *rr,
                                )),
                            )
                        }
                    } else if *ls > Self::DELTA * rs {
                        let Node { size: lls, .. } = ll.0.unwrap().0;
                        let Node {
                            key: lrk,
                            value: lrv,
                            size: lrs,
                            left: lrl,
                            right: lrr,
                        } = lr.0.unwrap().0;

                        if *lrs < Self::RATIO * lls {
                            node(
                                lk.clone(),
                                lv.clone(),
                                ls + rs + 1,
                                *ll,
                                Map::from(node(key, value, rs + lrs + 1, *lr, right)),
                            )
                        } else {
                            node(
                                lrk.clone(),
                                lrv.clone(),
                                ls + rs + 1,
                                Map::from(node(
                                    lk.clone(),
                                    lv.clone(),
                                    lls + lrl.size() + 1,
                                    *ll,
                                    *lrl,
                                )),
                                Map::from(node(key, value, rs + lrr.size() + 1, *lrr, right)),
                            )
                        }
                    } else {
                        node(key, value, ls + rs + 1, left, right)
                    }
                }
            },
        }
    }

    // This is supposed to be equivalent but it's not.
    // fn balance(
    //     key: K,
    //     value: V,
    //     left: Map<'r, K, V>,
    //     right: Map<'r, K, V>,
    //     arena: &Arena<Self>,
    // ) -> Gc<'r, Self> {
    //     let l_size = left.size();
    //     let r_size = right.size();
    //     let rl_size = l_size + l_size;
    //     let size = rl_size + 1;

    //     if rl_size <= 1 {
    //         arena.gc(Node {
    //             size,
    //             key,
    //             value,
    //             left,
    //             right,
    //         })
    //     } else if r_size > Self::DELTA * l_size {
    //         Node::rotate_left(key, value, left, right.0.unwrap(), arena)
    //     } else if l_size > Self::DELTA * r_size {
    //         Node::rotate_right(key, value, left.0.unwrap(), right, arena)
    //     } else {
    //         arena.gc(Node {
    //             size,
    //             key,
    //             value,
    //             left,
    //             right,
    //         })
    //     }
    // }

    // fn rotate_left(
    //     key: K,
    //     value: V,
    //     left: Map<'r, K, V>,
    //     right: Gc<'r, Self>,
    //     arena: &Arena<Self>,
    // ) -> Gc<'r, Self> {
    //     if right.left.size() < Self::RATIO * right.right.size() {
    //         Node::single_left(key, value, left, right, arena)
    //     } else {
    //         Node::double_left(key, value, left, right, arena)
    //     }
    // }

    // fn rotate_right(
    //     key: K,
    //     value: V,
    //     left: Gc<'r, Self>,
    //     right: Map<'r, K, V>,
    //     arena: &Arena<Self>,
    // ) -> Gc<'r, Self> {
    //     if left.right.size() < Self::RATIO * left.left.size() {
    //         Node::single_right(key, value, left, right, arena)
    //     } else {
    //         Node::double_right(key, value, left, right, arena)
    //     }
    // }

    // fn single_left(
    //     key: K,
    //     value: V,
    //     left: Map<'r, K, V>,
    //     right: Gc<'r, Self>,
    //     arena: &Arena<Self>,
    // ) -> Gc<'r, Self> {
    //     let left = arena.gc(Node {
    //         key,
    //         value,
    //         left,
    //         right: right.left,
    //         size: left.size() + right.left.size() + 1,
    //     });

    //     arena.gc(Node {
    //         key: right.key.clone(),
    //         value: right.value.clone(),
    //         size: left.size + right.size,
    //         left: Map(Some(left)),
    //         right: right.right,
    //     })
    // }

    // fn single_right(
    //     key: K,
    //     value: V,
    //     left: Gc<'r, Self>,
    //     right: Map<'r, K, V>,
    //     arena: &Arena<Self>,
    // ) -> Gc<'r, Self> {
    //     let right = arena.gc(Node {
    //         key,
    //         value,
    //         left: left.right,
    //         right,
    //         size: left.right.size() + right.size() + 1,
    //     });

    //     arena.gc(Node {
    //         key: left.key.clone(),
    //         value: left.value.clone(),
    //         size: left.size + right.size,
    //         left: left.left,
    //         right: Map(Some(right)),
    //     })
    // }

    // fn double_left(
    //     key: K,
    //     value: V,
    //     left: Map<'r, K, V>,
    //     r: Gc<'r, Self>,
    //     arena: &Arena<Self>,
    // ) -> Gc<'r, Self> {
    //     let rl = r.left.0.unwrap();

    //     let left = arena.gc(Node {
    //         key,
    //         value,
    //         left,
    //         right: rl.left,
    //         size: left.size() + rl.left.size() + 1,
    //     });

    //     let right = arena.gc(Node {
    //         key: r.key.clone(),
    //         value: r.value.clone(),
    //         left: rl.right,
    //         right: r.right,
    //         size: rl.right.size() + r.right.size() + 1,
    //     });

    //     arena.gc(Node {
    //         key: rl.key.clone(),
    //         value: rl.value.clone(),
    //         left: Map(Some(left)),
    //         right: Map(Some(right)),
    //         size: left.size + right.size,
    //     })
    // }

    // fn double_right(
    //     key: K,
    //     value: V,
    //     l: Gc<'r, Self>,
    //     right: Map<'r, K, V>,
    //     arena: &Arena<Self>,
    // ) -> Gc<'r, Self> {
    //     let lr = l.right.0.unwrap();

    //     let left = arena.gc(Node {
    //         key: l.key.clone(),
    //         value: l.value.clone(),
    //         left: l.left,
    //         right: lr.left,
    //         size: l.left.size() + lr.left.size() + 1,
    //     });

    //     let right = arena.gc(Node {
    //         key,
    //         value,
    //         left: lr.right,
    //         right,
    //         size: lr.right.size() + right.size() + 1,
    //     });

    //     arena.gc(Node {
    //         key: lr.key.clone(),
    //         value: lr.value.clone(),
    //         left: Map(Some(left)),
    //         right: Map(Some(right)),
    //         size: left.size + right.size,
    //     })
    // }

    // TODO benchmark specialized balance versions
    // fn balance_right(
    //     left: Map<'r, K, V>,
    //     right: Map<'r, K, V>,
    //     key: K,
    //     value: V,
    //     arena: &Arena<Self>,
    // ) -> Gc<'r, Self> {
    //     match left {
    //         None => match right {
    //             None => arena.gc(Map {
    //                 key,
    //                 size: 1,
    //                 left,
    //                 right,
    //                 value,
    //             }),
    //             Some(Gc(
    //                 Map {
    //                     left: None,
    //                     right: None,
    //                     ..
    //                 },
    //                 _,
    //             )) => arena.gc(Map {
    //                 key,
    //                 size: 2,
    //                 left: None,
    //                 right,
    //                 value,
    //             }),
    //             Some(Gc(
    //                 Map {
    //                     key: rk,
    //                     value: rv,
    //                     left: None,
    //                     right: Some(_),
    //                     ..
    //                 },
    //                 _,
    //             )) => arena.gc(Map {
    //                 key: rk.clone(),
    //                 size: 3,
    //                 left: Some(arena.gc(Map {
    //                     size: 1,
    //                     key,
    //                     value,
    //                     right: None,
    //                     left: None,
    //                 })),
    //                 right,
    //                 value: rv.clone(),
    //             }),
    //             Some(Gc(
    //                 Map {
    //                     key: rk,
    //                     value: rv,
    //                     left:
    //                         Some(Gc(
    //                             Map {
    //                                 key: rlk,
    //                                 value: rlv,
    //                                 ..
    //                             },
    //                             _,
    //                         )),
    //                     right: None,
    //                     ..
    //                 },
    //                 _,
    //             )) => arena.gc(Map {
    //                 key: rk.clone(),
    //                 size: 3,
    //                 left: Some(arena.gc(Map {
    //                     size: 1,
    //                     key,
    //                     value,
    //                     right: None,
    //                     left: None,
    //                 })),
    //                 right,
    //                 value: rv.clone(),
    //             }),
    //             _ => todo!(),
    //         },
    //         Some(_) => {todo!()}
    //     }
    //  }
}

#[derive(Trace, Clone, Eq, PartialEq)]
pub struct Iter<'r, K, V>
where
    K: 'r,
    V: 'r,
{
    parents: SmallVec<[Gc<'r, Node<'r, K, V>>; 32]>,
}

impl<'r, K: Trace, V: Trace> Iterator for Iter<'r, K, V> {
    type Item = (&'r K, &'r V);
    fn next(&mut self) -> Option<Self::Item> {
        self.parents.pop().map(
            |Gc(
                Node {
                    key, value, right, ..
                },
                _,
            )| {
                if let Map(Some(mut node)) = right {
                    self.parents.push(node);

                    while let Map(Some(left)) = node.left {
                        node = left;
                        self.parents.push(left);
                    }
                };

                (key, value)
            },
        )
    }
}

#[cfg(test)]
mod binary_tree_tests {
    use crate::collections::Map;
    use crate::*;
    use quickcheck_macros::*;
    use std::{collections::BTreeMap, fmt::Debug};

    fn get_set<K: Trace + Clone + Ord + Debug, V: Trace + Clone + Eq + Debug>(pairs: Vec<(K, V)>) {
        let sorted_pairs: BTreeMap<K, V> = pairs.iter().cloned().collect();

        let arena = Arena::new();
        let map_s = sorted_pairs.iter().fold(Map::default(), |map, (k, v)| {
            map.insert(k.clone(), v.clone(), &arena)
        });

        let map_r = pairs.iter().fold(Map::default(), |map, (k, v)| {
            map.insert(k.clone(), v.clone(), &arena)
        });

        sorted_pairs.iter().for_each(|(k, v)| {
            assert_eq!(v, &map_s[k]);
            assert_eq!(v, &map_r[k]);
        });
    }

    #[quickcheck]
    fn get_set_usize(pairs: Vec<(usize, usize)>) {
        get_set(pairs)
    }

    #[quickcheck]
    fn get_set_str(pairs: Vec<(String, usize)>) {
        get_set(pairs)
    }

    #[quickcheck]
    fn iter_usize(pairs: Vec<(usize, usize)>) {
        iter_test(pairs)
    }

    #[quickcheck]
    fn iter_str(pairs: Vec<(String, usize)>) {
        iter_test(pairs)
    }

    fn iter_test<K: Trace + Clone + Ord + Debug, V: Trace + Clone + Eq + Debug>(
        pairs: Vec<(K, V)>,
    ) {
        let sorted_pairs: BTreeMap<K, V> = pairs.iter().cloned().collect();
        let arena = Arena::new();
        let map_s = sorted_pairs.iter().fold(Map::default(), |map, (k, v)| {
            map.insert(k.clone(), v.clone(), &arena)
        });

        let map_r = pairs.iter().fold(Map::default(), |map, (k, v)| {
            map.insert(k.clone(), v.clone(), &arena)
        });

        let mut ri = map_r.iter();
        let mut si = map_s.iter();
        sorted_pairs.iter().for_each(|(k, v)| {
            assert_eq!(Some((k, v)), ri.next());
            assert_eq!(Some((k, v)), si.next());
        })
    }
}
